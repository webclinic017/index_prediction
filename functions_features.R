
target_nextday = function(s,prefix) {
  n=names(s)[names(s) %like% "Close"][1]
  out = ((s[,names(s) %like% "Close"] / s[,names(s) %like% "Open"]) -1 ) * 100
  out = c(out[2:length(out)],NA)
  out = matrix(out)
  colnames(out) = prefix %+% n %+% ".nday"
  out
}

direction_change = function(x) {
  out=rollapply(x, 2, function(x) {
    if (any(is.na(x)) ) {NA}
    else if (prod(x) < 0 ) {1} 
    else {0}
  })
  out = c(NA,out)
  out
}

create_updown = function(x){
  y=rep("down",length(x))
  y[x>0] = "up"
  y
}

target_minute_roll = function(d,window) {
  o=Delt(d,k=window) *100
  o=c(o[(window+1):length(o)],rep(NA,window))
  o
}



#-------5,15,60 min perday
target_minute = function(s,windows) {

  s$datetime=s$datetime %>% as.character %>% as.POSIXlt(.,tz="America/New_York")
  s$date = as.Date(s$datetime)
  # s$index = as.numeric(s$datetime)
  # 
  # # create full minute series per day
  # dlist=lapply(unique(s$date),function(x) {
  #   format(seq(as.POSIXlt(x %+% " 09:30:00", tz="America/New_York"),as.POSIXlt(x %+% " 16:00:00", tz="America/New_York") , by='1 min'))
  # }) 
  # dlist=do.call("c",dlist) %>%  as.vector
  # dlist= as.POSIXlt(dlist,tz="America/New_York")
  # tim_all = data.frame(datetime=dlist)
  # tim_all$index = as.numeric(tim_all$datetime)
  # 
  # # merge with NAs
  # new=merge(tim_all,s,all.x=T,by="index")
  # new = new[,!grepl("\\.y$|index",names(new))]
  # names(new)[1] = "datetime"
  # 
  # split into days
  dlist = split(s,s$date)
  
  # calc targets per list
  dlist = lapply(dlist,function(x) {
            n=which(!names(x) %like% "date|datetime|index")
            if (length(n) == 1)
                x=x[,n,drop=F]
            if (length(n) >= 2)
                x=x[,n]
            o=foreach(w=windows) %do% {
                t=lapply(x,function(y) {
                  target_minute_roll(y,w)
                })
                t = do.call("cbind",t)
                colnames(t) = colnames(t) %+% "." %+% w
                t
            }
            o=do.call("cbind",o)
            o
            })
  dlist = do.call("rbind",dlist)
  dlist
}


# date is day for predction, so start with date - 1
extract_tec_indicators = function(symb,stock.ohcl,day,nback,daysselect) {
  
  #debug
  # s = st
  # daysselect = c(1,5,10)
  # nback = 14
  # day = index(st) %>% as.character %>% as.Date %>% tail(1)
  
  # expected column output features
  so = stock.ohcl
  ncol_base = 17
  out = rep(NA,ncol_base*length(daysselect))
 
  # select days
  so = so[index(so) %>% as.character %>% as.Date <= day,1:5]
  names(so) = c("Open","High","Low","Close","Volume")
  
  # if stock is longer than 100 days then calculate tec indicators
  if(nrow(so) >= 100  ) {
    
    fsbase = list(
      #------------5) Volume weights – OBV
      # on balance volume
      obv.vol = OBV(so$Close,so$Volume),
      
      #----------1) Price change – ROCR, MOM
      #Price Rate of Change
      roc.usd = ROC(so[,"Close"]),
      mom.usd = momentum(so[,"Close"]),
      
      #------------4) Volatility signal – ATR
      #bolling bands
      #bbands.usd = BBands(so$Close),
      
      # Moving Average Convergence Divergence
      macd.long.usd = MACD( so[,"Close"], 12, 26, 9)[,1],
      macd.shortg.usd = MACD( so[,"Close"], 3, 6, 2)[,1]
    )
    
    fsext=list(
      # Williams %R
      wpr.x.hcl = WPR(so[,c("High","Low","Close")],n=nback),
      
      # Average true range
      atr.x.hcl = ATR(so[,c("High","Low","Close")], n=nback)[,1:2],
      
      #-----------2) Stock trend discovery – ADX, MFI
      # A Directional movement index
      adx.x.hcl = ADX(so[,c("High","Low","Close")], n = nback),
      
      # Trend detecion index
      tdi.x.usd = TDI(ROC(so[,"Close"]),n=nback)[,1],
      
      #-----------3) Buy&Sell signals – WILLR, RSI, CCI, MACD
      #Relative Strength Index
      reli.x.vol = RSI(so$Close, n=nback, maType="WMA", wts=so[,"Volume"]),
      reli.x.usd = RSI(so$Close, n=nback),
      
      # Chande Momentum Oscillator
      cm.x.osc.usd= as.vector(CMO(so$Close,n=nback)),
      cm.x.osc.vol= as.vector(CMO(so$Volume,n=nback))
    )
    names(fsext) = gsub("\\.x\\.","." %+% nback %+% ".",names(fsext))
    
    #----combine
    fs=c(fsbase,fsext)
    
    #----varnames
    for (i in 1:length(fs)) {
      names(fs[[i]]) = names(fs)[i] %+% "." %+% names(fs[[i]])
    }
    
    #---impplode
    fs=do.call("cbind",fs)
    fs = as.data.frame(fs)
    fs=fs[rev(1:nrow(fs)),]
    fs
    names(fs) = "f.tec_" %+% names(fs)
    
    #------expand through daysback
    # o = list()
    # for (d in daysselect) {
    #   new = fs[1+d,]
    #   names(new) = names(new) %+% "_" %+% d
    #   o[[length(o) + 1]] =  new
    # }
    # o = do.call("cbind",o)
    # rownames(o) = rownames(fs[1,])
    #out = o
    out=fs[1,]
  }
  
  return(out)
}


#-----------generate day difference features: requires desc time order descein
build_features_day_perc <- function(objDF,days, offLimitsSymbols,symbs, roundByScaler) {
  # needs to be sorted by date in decreasing order
  objDF = objDF[,symbs]
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    #if (!sym %in% offLimitsSymbols) {
      print(paste('********theColName', sym))
      for (day in days) {
        objDF[paste0("f.perc_" %+% sym,'_',day)] <- round((c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1) / objDF[,sym] *100,roundByScaler) 
      }
    #}
  }
  return (objDF[,!names(objDF) %in% symbs])
}







#--------generate features same day difference in %
build_features_samedayOC = function(d,symbs) {
  
 out =  symbs %>% sapply(function(x) (d[,x %+% ".Close"] / d[,x %+% ".Open"] -1 ) * 100 )  
 if (is.null((dim(out)))) names(out) = "f.dayoc_" %+% names(out) else colnames(out) = "f.dayoc_" %+% colnames(out)
 out
}

build_features_samedayMinMax = function(d,symbs) {
  
  out =  symbs %>% sapply(function(x) (d[,x %+% ".High"] / d[,x %+% ".Low"] -1 ) * 100 )
  colnames(out) = "f.dayminmax_" %+% colnames(out)
  out
}


#--------generate features momentum
build_features_momentum  = function(OPCLO,days) {
  OPCLO = OPCLO %>% as.data.frame
  OPCLO[is.na(OPCLO)] <- 0
  out = foreach (d=days) %do% {
    o=rollapply(OPCLO,FUN=mean,width=d,by=1,fill = NA,align="left")
    o
  } %>% do.call("cbind",.)
  colnames(out) = "f_mom_" %+% days
  out
}




#---------features month, day, week from date
build_features_dwm = function(date) {
  d=date
  o=data.frame(f.time_day=weekdays(d),f.time_month=format(d,"%m") )
  dv=dummyVars(~ .,o)
  predict(dv,o) %>% as.data.frame
}


#--------features rolling indicator
# OLD!
build_features_leadindic = function(allSymbols,targetSymbol,lag,d) {
  print("...bulding leading indicators " %+% targetSymbol %+% " lag: " %+% lag)
  oc = build_features_samedayOC(allSymbols,d) %>% as.data.frame
  target= names(oc)[names(oc) %like% targetSymbol]
  target_do = shift(oc[,target],1,type="lag")
  target_up = shift(oc[,target],1,type="lead")
  
  oc_prod = oc %>% sapply(function(x) x*target_up) %>% as.data.frame
  lead_score=vector()
  lead_prob = vector()
  lead_list = list()
  lead_num = vector()
  past_score = vector()
  for (i in 1:nrow(oc)) {
      # only caclulate leading indications for obervations i+1 (df sorted descending), otherwise bias
      prop.samedir = oc_prod[(i+1):nrow(oc_prod),] %>% sapply(function(x) mean(ifelse(x > 0,1,0)[1:lag]) )%>% sort(decreasing=T)
      if (length(prop.samedir) > 0) {
        max_lead = prop.samedir[prop.samedir == max(prop.samedir) %>% as.vector]
        past_prob=ifelse(target_do[(i+1):(i+1+lag)] > 0,1,0) %>% mean
        lead_dir = oc[i,max_lead %>% names,drop=F]
        lead_dir = ifelse(lead_dir > 0,1,0) %>% mean
        past_score =  c(past_score,past_prob)
        lead_score = c(lead_score,lead_dir * (max_lead %>% max))
        lead_prob = c(lead_prob,max_lead %>% max)
        lead_num = c(lead_num,max_lead %>% length)
        lead_list[[length(lead_list) + 1]] = max_lead %>% names
        
      } else {
        past_score =  c(past_score,NA)
        lead_score = c(lead_score,NA)
        lead_prob = c(lead_prob,NA)
        lead_num = c(lead_num,NA)
        lead_list[[length(lead_list) + 1]] = NA
        }
  }
  out=data.frame(past_score=past_score,lead_score=lead_score,lead_prob=lead_prob,lead_num=lead_num)
  names(out) = c("f.past_score_","f.leadindic_score_","f.leadindic_prob_","f.leadindic_num_") %+% targetSymbol %+% "_" %+% lag
  list(features=out,lead_list=lead_list)
}


#--------features leading indicator V2: no NAs!!!
build_features_leadindic = function(d,target_vector,windows,thres,maxlines) {
    MAX_NAS_IN_TARGET = 10
    out = data.frame()
    d=sf[,predictorNames]
    
    #debug
    # windows = c(100)
    # thres = c(0.6)
    # target_vector = sf$target.pure
    # maxlines = 30
    #cbind(sf$target.pure[1:50],out)
    # i = 17
    
    for (i in 1:maxlines) {
        
        print(i)
        row=vector()
        for (w in windows) {
          ds = d[(i+1):(i+1+w),]
          ts =  target_vector[(i+1):(i+1+w)]
          features = sapply(ds,function(x) {
              dirprod = x * ts
              dirprod = mean(ifelse(dirprod > 0,1,0),na.rm=T)
              dirprod
          }) %>% sort %>% rev
          
            for (v in thres) {
                if (length(features) > 0 & max(features) >= v &  sum(is.na(ts)) <= MAX_NAS_IN_TARGET) {
                  
                      values = as.numeric(ds[1,names(features[features >=v])])  
                      
                } else {values = 0}
                #val = values  %>% median
                val = mean(ifelse(values > 0,1,ifelse(values < 0,-1,0)),na.rm=T)
                #val = values %>% median
                if (is.nan(val)) val=NA
                res = list(f.leadindic_w=val)
                names(res) = names(res) %+% w %+% "_" %+% v
                row=c(row,res)
            }
        }
        row = row %>% as.data.frame
        out = rbind(out,row)
    }
    out
}






#-----------return top3 cross correlations per time series
Find_Max_CCF<- function(a,b,date)
{
  x <- ts(data.frame (date,a) %>% arrange(date))
  y <- ts(data.frame(date,b) %>% arrange(date))
  d <- ccf(x[,2], y[,2],plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res = res %>% arrange(desc(abs(cor)))
  res_max = res[1:3,]
  return(res_max)
} 


#-------------corellation confidence intervalls
build_cors_ci = function(df,target) {
  
  
  cl=lapply(names(df),function(x) {
    ct=cor.test(target, df[,x], method = "pearson", conf.level = 0.95)
    cbind(x,ct$p.value,ct$estimate,ct$conf.int[1],ct$conf.int[2])
    
  })
  cl=as.data.frame(do.call("rbind",cl))
  rownames(cl) = NULL
  cl=cl[!is.na(cl[,2]),]
  cl = clean_df(cl)
  names(cl) = c("var","p","cor","lower","upper")
  cl[,2:5]=apply(cl[,2:5],2,round,3)
  cl$ci_diff= abs(abs(cl$lower) - abs(cl$upper))
  cl$overlap = cl$lower*cl$upper
  cl$var = as.character(cl$var)
  
  # remove insig and overlappers
  cl=subset(cl,p<= 0.05 & overlap > 0)
  cl$cor = abs(cl$cor)
  cl=cl[order(-cl$cor),]
  rownames(cl) = NULL
  cl
}

#------------clean data frame
clean_df = function(x) {
  x0=x
  x = data.frame(lapply(x,as.character),stringsAsFactors=FALSE)
  x = data.frame(lapply(x,as.numeric),stringsAsFactors=FALSE)
  nas= unlist(lapply(x,function(x) all(is.na(x))))
  nas=as.vector(which(nas == TRUE))
  x[,nas] = x0[,nas]
  x[,-nas] = data.frame(lapply(x[,-nas],as.numeric))
  x
}

# impute values, data must be ascending
impute_time_NAs = function(df) {
  ratios = apply(df,2,function(x) {length(which(is.na(x))) / length(x)})
  for (n in names(df)[!names(df) %like% "date|target"] ) {
    nas = which(is.na(df[,n]))
    if (length(nas) > 0) {
      print(n)
      # replace leading NAs
      i=2
      while (is.na(df[1,n])) {
        df[1,n] = df[i,n]
        i = i + 1
      }
      df[,n] = na.locf(df[,n])
    }
  }
  df
}

#-----------------------Facebook Prophet
build_facebook_prophet_nextday = function(df,y,prefix,periods_train,periods_predict) {
  out = data.frame()
  for (p in periods_train) {
    tse = df %>% tail(p)
    tp = data.frame(ds=tse$date, y=tse[,y])
    m=prophet(tp,yearly.seasonality = FALSE, weekly.seasonality= FALSE)
    fdf=make_future_dataframe(m, periods = periods_predict)
    f <- predict(m, fdf ) %>% tail(1)
    today = tse[,y] %>% tail(1)
    feat = which(names(f) %like% "yhat|trend_")
    f[,feat]=sapply(f[,feat],function(x) stockdiff(today,x) )
    names(f) = names(f) %+% "_" %+% p
    f=f[1,c(2,nout)]
    if (nrow(out) == 0) out=rbind(out,f) else out=cbind(out,f)
  }
  names(out) = prefix %+% names(out)
  out$date = tse[,"date"] %>% tail(1)
  out
}

#-----------------calculate stock percentage difference (%)
stockdiff = function(open,close) {
  (close / open  -1 ) * 100
}

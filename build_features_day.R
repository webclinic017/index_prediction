

  print("-----------------------------------loading data")
  #--------------load day stock file with all stocks, order desc by date
  s=readRDS("stocks_day/stocks_combined_" %+% targetSymbol %+% ".rds")
  all_symbs_close = s[,names(s) %like% "Close"] %>% names %>% get_name
  
  build_features_all = function(s,all_symbs_close,targetSymbol) {

  
  print("-----------------------------------impute values: "%+% targetSymbol)
  
  #------remove first leading NAs
  s <- s %>% arrange(date)
  NAs = apply(s,1,function(x){length(which(is.na(x)))})
  start = which(NAs == min(NAs[1:250]))[1]
  s = s[start:nrow(s),]
  dim(s)
  tail(s)
  
  #------impute NAs
  s = impute_time_NAs(s[1:(nrow(s)),])
  s %>% tail(10)
  print("-----------------------------------start building features: "%+% targetSymbol)
 
  #-------reorder descending
  s %>% dim
  s <- s %>% arrange(desc(date))
  head(s)
  
  #--------------load dates of target to normalize to
  s$targetNA = is.na(s[,targetSymbol %+% ".Close"]) %>% as.numeric
  s$target_Open = s[,targetSymbol %+% ".Open"]
  s$target_Close = s[,targetSymbol %+% ".Close"]
  
  #------Targets and SHIFT in future ordered desc
  TARGETS = s[,names(s) %like% "date" | names(s) %like% "target"]
  STOCK_TODAY = data.frame(today_Close=TARGETS$target_Close, today_Open=TARGETS$target_Open)
  TARGETS[,2:ncol(TARGETS)] = sapply(TARGETS[,2:ncol(TARGETS)],FUN=shift,n=1,type="lag")

  #------ Open and Close rates same day 9-12 and 12-16 (only for US stocks)
  TS_OPCLO = data.frame()
  # dmin = readRDS("stocks_minute/GSPC.rds") %>% filter(hour(datetime) == 12 & minute(datetime) == 0 ) %>% head(1) # DUMMY!!!
  # tryCatch({ dmin = readRDS("stocks_minute/" %+% targetSymbol %+% ".rds")} ,error=function(e) NULL)
  # dmin %>% dim
  # dmin = dmin %>% filter(hour(datetime) == 12 & minute(datetime) == 0 )
  # dmin$target_12 = dmin[,6]
  # TS_OPCLO = merge(s,dmin[,c("date","target_12")],by = "date",all.x=T)
  # TS_OPCLO = TS_OPCLO %>% arrange(desc(date))
  # #-------same day difference 9:30 - 12:00 [f.sameday]
  # TS_OPCLO$f.sameday_912 = (TS_OPCLO$target_12 / TS_OPCLO$target_Open - 1) * 100
  # TS_OPCLO=TS_OPCLO[,c("target_Open","target_Close","target_12","f.sameday_912")]
  
  #-------same day difference Open Close [f.dayoc]
  OPCLO = build_features_samedayOC(s,all_symbs_close)

  #-------momentum [f.mom]
  MOM = build_features_momentum(OPCLO[,"f.dayoc_" %+% targetSymbol,drop=F],days=c(12))
  
  #--------min max [f.dayminmax]
  MINMAX = build_features_samedayMinMax(s,all_symbs_close)
  
  # #------specific technial indicators [ f.tec]
  TEC_IND =data.frame()
  # target_s = s[,(!names(s) %like% "f\\." & names(s) %like% targetSymbol)]
  # target_s=target_s %>% sapply(Hmisc::impute,'random')              # impute missings
  # rownames(target_s) = s[,"date"] %>% as.character
  # target_s = target_s %>% as.xts                                     # convert XTS
  # print("....build technical indidcators...")
  # for (day in s$date) {
  #   day = day %>% as.Date
  #   print(day)
  #   ti = extract_tec_indicators(targetSymbol,stock.ohcl=target_s,day=day,nback=14,daysselect=c(1))
  #   TEC_IND=rbind(TEC_IND,ti)
  # }
  # 


  #------general percentage feature [f.perc]
  PERC = build_features_day_perc(s, days=c(1,2,3,4),symbs=names(s)[names(s) %like% "\\.Close$|Volume"],offLimitsSymbols=NA, roundByScaler=10)
  
  #------per cmooths
  ss = s[,names(s) %like% "\\.Close$|date"]  %>% arrange(date)
  ss = sapply(ss[,names(ss) != "date"],SMA,n=20 )  %>% as.data.frame %>% rev_me
  PERC_SMOTH = build_features_day_perc(ss, days=c(30,90),symbs=names(ss),offLimitsSymbols=NA, roundByScaler=10)
  names(PERC_SMOTH) = names(PERC_SMOTH) %+% "_smooth"
  
  #------percentage change for tec indicaotrs [f.perc_f.tec]
  #PERC_TEC = build_features_day_perc(TEC_IND, days=c(1,2,3,4,5,7,14,21),symbs=names(TEC_IND),offLimitsSymbols=NA, roundByScaler=10)
  
  #--------day features [f.time_s]
  #F_TIME = build_features_dwm(s$date) 
  #F_TIME = F_TIME[,!names(F_TIME) %like% "month"]
  
  #------Google Trends
  
  #------Twitter
  
  #-------PutCall Options
  PCR = s[,names(s) %like% "_pcr$"] 

  
  #------COMBINE FEATURES
  s4 = cbind(OPCLO,MOM,MINMAX,PERC,PERC_SMOTH,PCR
             #,TS_OPCLO
             #,TEC_IND,PERC_TEC
             #,F_TIME
             )

  #------MERGE TARGETS
  s4 = cbind(STOCK_TODAY,TARGETS, s4)
  
  #-----TODAY_OPEN
  s4$f.perc_TODAYOPEN_Close_1 = stockdiff(s4$today_Close,s4$target_Open)
  
  #-----Combine forecasts
  #FORE = readRDS("stocks_day/stocks_forecasts_" %+% targetSymbol %+% ".rds")
  #FORE = FORE[,!names(FORE) %like% "prophet_t_"]
  #s4 = merge(s4,FORE,by="date",all.x=TRUE)

  #-----order asc
  s4 = s4 %>% arrange(date)
  
  #-----remove first OBS
  s4 = s4[120:nrow(s4),]
  
  #------impute per time
  
  # #--------descending
  s4 = s4 %>% arrange(desc(date) )

  #-------------SHIFTS all JAPAN AND AUSTRALIA STOCKS 1 day to future (Time Zone!)
  #JAP_AUST = s4[,names(s4) %like% "AXJO|N225"]
  #JAP_AUST = sapply(JAP_AUST,FUN=shift,n=1,type="lag")
  #s4 = s4[,!names(s4) %like% "AXJO|N225"]
  #s4= cbind(s4,JAP_AUST)
  #s4 %>% head
  
  #----------remove 0 variance features or 20% NAs
  # nzv <- nearZeroVar(s4, saveMetrics = TRUE)
  # Nas = sapply(s4,function(x) length(which(is.na(x)))) 
  # nzv$nas = Nas
  # nzv$var = rownames(nzv)
  # nzv[nzv$zeroVar == TRUE | nzv$nzv  == TRUE | nzv$nas > 0,]
  # keep.col = nzv[nzv$nzv == FALSE & nzv$zeroVar == FALSE  & nzv$nas <= 200 | nzv$var %like% "target",]$var
  # s5 = s4[,keep.col]
  # print("cleaning up...")
  # print("before :" %+% dim(s5))
  # keep.row = apply(s5,1,function(x) length(which(is.na(x)))) %>% as.vector < 40
  # sum(!keep.row)
  # s5 = s5[keep.row,]
  # print("final :" %+% dim(s5))
  #s6 = s4
  
  #----------Cut beginning NAs
  #first0NA = which(apply(s6[,names(s6) %like% "f."],1,function(x) sum(is.na(x)) == 0 ))[1] %>% names %>% as.numeric
  #s6 = s6[first0NA:nrow(s6),]
  #s7=s6
  s7=s4
  
  #------------TARGET Next day (ASC)
  s7$target_1.pure = (s7$target_Close / s7$target_Open - 1) * 100
 
  #------------TARGET Next day 12-16
  #s7$target_1_1216.pure = NA
  #s7$target_1_1216.pure = (s7$target_Close / s7$target_12 - 1) * 100
  #s7$target_1_1216.pure = shift(s7$target_1_1216.pure,1,type="lead")
  #s7[1:10,names(s7) %like% "target|date|GSPC.Close|GSPC.Open|f.dayoc_GSPC"]

  #------------TARGET 7 days
  # dates = s7[,"date",drop=F]
  # dates$date7 = dates$date + 7
  # price = s7[s7$date %in% dates$date7 ,c("date","target_Close")]
  # new = merge(dates,price,by.x = "date7",by.y = "date",all.x = TRUE )
  # names(new)[3] = "target_Close7"
  # s7 = merge(s7,new[,c("date","target_Close7")],by.x = "date",by.y = "date",all.x = TRUE )
  # s7$target_7.pure = (s7$target_Close7 / s7$target_Close - 1) * 100
  
  #-----date
  s7$date = s7$date %>% as.character %>% as.Date
  s7
  }
  
  #---------FINAL BUILD
  s7 = build_features_all(s,all_symbs_close,targetSymbol)
  sf = s7

  #----------PREPARE TARGETS
  target = "target_1.pure"
  sf$target_delta = sf[,target]
  sf$target_bin <- ifelse(sf[,target] > 0, 1, 0)
  sf$target_sym <- ifelse(sf[,target] > 0, 1, -1)
  sf$target <- cut(sf[,target], breaks=c(-Inf,0, Inf), labels=c("down_stay","up_change"))
  sf$today_delta = ((sf$target_Close / sf$today_Open) -1) * 100
  
  testdata = c("date","today_Open","today_Close","target_Open","target_Close",
               "today_delta","target_delta","target_bin","target_sym","target")
  
  #--------------Save
  write.csv(sf, file = "data_training/" %+% targetSymbol %+% ".csv",na = "NA", row.names = FALSE, fileEncoding = "utf-8")
  write.csv(sf[,testdata], file = "data_test/" %+% targetSymbol %+% ".csv", na = "NA", row.names = FALSE, fileEncoding = "utf-8")

  #--------------Save
  saveRDS(sf,file = "stocks_day/stocks_features_" %+% targetSymbol %+% ".rds")


source("unit_test_target.R")
sf %>% dim
print(sf$date %>% head(10))



#https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/


score = function(t,targetSymbol,n_train,model) {
  # n_train=1
  # t=result
  # cut = 0.5
  t$date = t$date %>% as.character %>% as.Date
  count_months = abs(elapsed_months(min(t$date ),max(t$date )))
  days.test = t %>% nrow
  pred <- prediction(t$probs, as.factor(t$target))
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  auc = performance(pred, measure = "auc")@y.values[[1]]
  proc.perf = pROC(pred, fpr.stop=0.2)
  cut_perfect=opt.cut(proc.perf, pred)[3,]
  cutoffs = c(0.5,0.6,0.7,cut_perfect) %>% as.numeric
  #t = t[!is.na(t$probs),]
  out = data.frame()

  for (cut in cutoffs) {
    tf = t
    if (tf %>% nrow >= 2 ) {
      tf$hit = get_hits_fp(cut,tf)
      if (nrow(na.omit(tf)) > 0 ) {
        tf = tf[!is.na(tf$hit),]
        tf$pred = tf$target
        down = tf$target == "up_change" & tf$hit == 0
        up = tf$target == "down_stay" & tf$hit == 0
        if (!all(!down)) tf[down,]$pred = "down_stay"
        if (!all(!up)) tf[up,]$pred = "up_change"
        u = c("up_change","down_stay")
        cm=confusionMatrix(table(factor(tf$pred %>% as.character,u), factor(tf$target %>% as.character,u)))
        hit = tf[tf$pred == tf$target,] %>% nrow
        miss = tf[tf$pred != tf$target,] %>% nrow
        money= hit * 20 * 0.8 - miss*20*1
        res=data.frame(months=count_months,stock=targetSymbol,n.train=n_train,auc=auc,cutoff=cut,
                       accuracy=cm$overall["Accuracy"],
                       pos.pre.val=cm$byClass["Pos Pred Value"],
                       neg.pre.val=cm$byClass["Neg Pred Value"],
                       precision=cm$byClass["Precision"],
                       days.test=days.test,
                       days.trade=tf %>% nrow,
                       money20=money,
                       money20.day=round(money/days.test,1),
                       trend.up=mean(t$target_bin)
        )
      }
      out = rbind(out,res)
    }
  }
  rownames(out) = NULL
  out$model = model
  out
}

returns_binary = function(INVEST_DAY,result,cutoff) {
  result$hit = get_hits_fp(cutoff,result)
  result$binary = 0
  result[result$hit == 1 & !is.na(result$hit),]$binary = INVEST_DAY*0.8
  result[result$hit == 0 & !is.na(result$hit),]$binary = -INVEST_DAY
  trades = sum(!is.na(result$hit))
  invested = trades * INVEST_DAY
  result$cum= cumsum(result$binary) + invested
  result$delta = (Delt(result$cum,k=-1) *100) %>% as.vector
  result$delta[1] = ((invested + result$hit[1]) / invested - 1) * 100
  result$cum
}

returns_cfd = function(INVEST_TRADE,cutoff,result,FEE_PERC,FEE_FIXED) {
  result$hit = get_hits_fp(cutoff,result)
  result$cfd = abs(result$real) / 100 + 1
  if (length(which(result$hit == 0)) > 0 )
    result[result$hit == 0 & !is.na(result$hit),]$cfd = 2-abs(result[result$hit == 0 & !is.na(result$hit),]$cfd)
  if (length(which(is.na(result$hit))) > 0 )
    result[is.na(result$hit),]$cfd = 1
  v=result$cfd
  v[1] = (v[1] * INVEST_TRADE) - (INVEST_TRADE * FEE_PERC + FEE_FIXED  )
  for (i in 2:length(v)) {
    v[i] = (v[i-1] * v[i]) - (v[i-1]* FEE_PERC + FEE_FIXED )
  }
  round(v,1)
}

returns_buyhold = function(INVEST_TRADE,result) {
  v= round(result$target_Close / result$target_Close[1] * INVEST_TRADE,0)
  v
}

get_hits_fp = function(cutoff,result) {
  result$hit = NA
  tryCatch({
  result[(result$real < 0 & result$probs > cutoff) | (result$real >= 0 & result$probs < (1-cutoff)),]$hit = 0
  result[(result$real >= 0 & result$probs > cutoff) | (result$real < 0 & result$probs < (1-cutoff)),]$hit = 1
  },error = function(e) {})
  result$hit
}



get_sharp = function(money_cum) {
  delta = (Delt(money_cum,k=-1) *100) %>% as.vector
  delta[1] = 0
  sqrt(delta %>% length) * (mean(delta)/sd(delta) )
}

get_kpis = function(result,money_cum) {
  o = list()
  o$sharp = get_sharp(result[,money_cum]) %>% round(1)
  o$invested = result[1,money_cum]
  o$ROI= (result[,money_cum] %>% tail(1) / o$invested *100 - 100) %>% round(1)
  o$trades = sum(!is.na(result$hit))
  o$time_years = as.numeric(difftime(result$date %>% tail(1) %>% as.Date, result$date[1]  
                                     %>% as.Date, unit="days")) / 365
  o$time_years_rnd = o$time_years %>% round(2)
  o$ROI_year = paste((Delt(result[,money_cum][seq(1,nrow(result),250)])[,1] %>% na.omit * 100) %>% round(1),collapse = " | ")
  o
}





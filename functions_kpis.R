

load_predictions = function(M_SELECT) {
  M_TARGET = strsplit(M_SELECT,"_")[[1]][1]
  test=read.csv("data_test/" %+% M_TARGET %+% ".csv")
  pred = read.csv("models_prediction/" %+% M_SELECT)
  d=merge(test,pred ,by="date",all.x = F, all.y=T)
  d
  
}

make_pred = function(probs,cutoff) {
  pred = ifelse(probs >= cutoff,1,ifelse(probs < (1-cutoff),0,NA))
  pred
}

get_kpis_ML = function(cutoff,d) {
  
  d$pred = make_pred(d$probs,cutoff)
  cm=confusionMatrix(d$pred,d$target_bin,positive = "1")
  o=data.frame(
    cutoff=cutoff,
    accuracy=cm$overall["Accuracy"],
    pos.pre.val=cm$byClass["Pos Pred Value"],
    neg.pre.val=cm$byClass["Neg Pred Value"],
    precision=cm$byClass["Precision"]
  )
  o['days'] = nrow(d)
  o['days.trade'] = nrow(na.omit(d))
  rownames(o) = NULL
  o
  
}

get_kpis_bestcut = function(d,fpr.stop) {
  
  pred <- prediction(d$probs,factor(d$target_bin))
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  auc = performance(pred, measure = "auc")@y.values[[1]]
  proc.perf = pROC(pred, fpr.stop=fpr.stop)
  cut_perfect=opt.cut(proc.perf, pred)[3,]
  cut_perfect
}

get_kpis_cutoffs = function(d) {
  o=data.frame()
  bestcut = get_kpis_bestcut(d,fpr.stop = 0.15)
  cuts = sort(c(bestcut,0.5,0.55,0.6,0.65,0.7,0.75,0.8))
  for (cu in cuts) {
    o=rbind(o,get_kpis_ML(cu,d))
  }
  o
}

create_returns = function(d,INVEST=1000,cutoff=0.5,FEE_LONG=0.0001,FEE_SHORT=0.0002,FEE_FIX=1) {
  
  d$pred = make_pred(d$probs,cutoff)
  d$fee_comm = ifelse(is.na(d$pred),0,ifelse(d$pred == 1,FEE_LONG,FEE_SHORT))
  d$pred_return = ifelse(is.na(d$pred),0,ifelse(d$pred == d$target_bin,abs(d$target_delta),-abs(d$target_delta)))
  d$returns = 0
  d$returns[1] = INVEST + (INVEST * d$pred_return[1] / 100 * 2 * (1-d$fee_comm[1]) ) - FEE_FIX
  
  for (i in c(2:nrow(d))) {
    money=d$returns[i-1] 
    if (!is.na(d$pred[i])) d$returns[i] = (money+ (money * d$pred_return[i] / 100) * (1-d$fee_comm[i])^2) - FEE_FIX
    else d$returns[i] = money
  }
  d$returns_delta = Delt(d$returns)[,1] # LATER / EARLIER - 1
  d$returns_delta[1] = (d$returns[1] / INVEST) - 1
  d$returns_perc = d$returns_delta*100
  rownames(d) = as.Date(d$date)
  out=d[,c("returns","returns_delta","returns_perc")]
  return(out) 
  
}


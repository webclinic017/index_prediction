#-----TEST
sf = readRDS("stocks_day/stocks_features_" %+% targetSymbol %+% ".rds") %>% arrange(desc(date) )
sf.names = c("date","f.dayoc_N225","f.dayoc_FTSE","target_1.pure","target_Open","target_Close")
rand_i = sample(3:(nrow(sf)-3),1)
t = sf[rand_i:(rand_i+1),] %>% arrange(desc(date))
t1=equals(t$target_Open[2],t[1,targetSymbol %+% ".Open"])
t2=equals(t$target_Close[2],t[1,targetSymbol %+% ".Close"])
t3=equals(t$target_1.pure[2],(t[1,targetSymbol %+% ".Close"] / t[1,targetSymbol %+% ".Open"] - 1) * 100 )
if (all.equal(t1,t2,t3)) print("Test target movement OK") else {
  print("Test target movement FAIL")
  print(t)
}


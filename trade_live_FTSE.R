targetSymbol="FTSE"

# http://www.marketindex.com.au/asx200

#------------------------LIVE NEXT DAY PREDICTION

#-------prep and load
test = readRDS("stocks_day/stocks_features_" %+% targetSymbol %+% ".rds")
test = test %>% arrange(desc(date))
test = test[1,c("date",features)]
test


up <- predict(object=mlmodel_glmnet, test[,features], type='prob')[,"up_change"]
down <- predict(object=mlmodel_glmnet, test[,features], type='prob')[,"down_stay"]
probs_glmnet=up/(up+down)
probs_glmnet
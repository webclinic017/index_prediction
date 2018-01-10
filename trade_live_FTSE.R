targetSymbol="FTSE"
# http://www.marketindex.com.au/asx200

#------------------------LIVE NEXT DAY PREDICTION
source("build_traindata.R")

#------load data
test = readRDS("stocks_day/stocks_features_" %+% targetSymbol %+% ".rds")
test = test %>% arrange(desc(date))

#-----load model
NAME_MODEL= "FTSE_R_glment_1050_25_0.5"
model = readRDS("models/" %+% NAME_MODEL %+% ".rds")
features = readRDS("models/" %+% NAME_MODEL %+% "_features.rds")

#-----predict
test = test[1,c("date",features)]
print(test)

up <- predict(object=model, test[,features], type='prob')[,"up_change"]
down <- predict(object=model, test[,features], type='prob')[,"down_stay"]
probs=up/(up+down)

print(probs)

#------>>> Call Trading Scripts
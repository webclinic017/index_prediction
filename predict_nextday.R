Sys.setenv(TZ='America/New_York')
"%+%" <- function(x,y) {paste(x,y,sep="")}
source("functions_general.R")
source("functions_features.R")
source("functions_eval.R")
library(glmnet)

#-------------SETTINGS for rolling window and training
targetSymbol = "FTSE"
train.days=1050
test.days=25
train_threshold = 0.5    # only train obs where delta is higher 0.5%
horizon = 20
window = 1000
days_backtesting = 1000

#---------------------cluster
#stopCluster(cl)
#rm(cl)
if (!exists("cl")) {
  cl <- makeCluster(5,outfile=getwd() %+% "/logs/clusterwork_" %+% as.Date(Sys.time()) %+% ".txt")
  registerDoParallel(cl)
}

#--------------# LOAD STOCK-----------
print("starting " %+% targetSymbol)
sf = readRDS("stocks_day/stocks_features_" %+% targetSymbol %+% ".rds")
sf = sf %>% arrange(date) 
sf %>% tail(5)

#---------------remove first line (nextday) and select where target is available
sf = sf[!is.na(sf$target_delta),] 

#--------------let's model
predictorNames = names(sf)[names(sf) %like% "f\\."] # all
#predictorNames = names(sf)[names(sf) %like% "AXJO|N225|GSPC|VIX"]  # FTSE
predictorNames = names(sf)[names(sf) %like% "AXJO|N225|JPY=X"]  # FTSE FUTURE
predictorNames = predictorNames[!( predictorNames %like% "daymin|_30$|_4$|smooth|shift")]

#----parameteres
max(sf$date) - min(sf$date)
sf %>% dim  
sf = sf %>% arrange(desc(date))
back=best_gap=0
testseq=seq(0,days_backtesting,test.days) 
tuneGrids=list()
tuneGrids[["glmnet"]] = glm_grid=expand.grid(.alpha = c(0,1),.lambda = seq(0,0.1,0.025))
tuneGrids[["glmnet"]] = glm_grid=expand.grid(.alpha = c(0),.lambda = seq(0,0,0))
features = c(predictorNames) %>% unique

result = data.frame()

for (back in testseq ) { 

  #-------test and trainng data
  print(back %+% " of " %+% testseq %>% tail(1))
  ro=back 
  split.te <- c((1+ro):(test.days+ro))
  split.tr <- c((test.days+ro+1):(test.days+ro+1+train.days))
  test <- sf[split.te,]
  train <-sf[split.tr,] 
  trainCLEAN = train
  dim(trainCLEAN)
  trainCLEAN$target.2 = trainCLEAN$target
  trainCLEAN$target = trainCLEAN$target %>% as.character
  trainCLEAN[abs(trainCLEAN$target_delta) < train_threshold,"target"] = "noise"
  trainCLEAN$target = trainCLEAN$target %>% as.factor
  #print(describe(trainCLEAN$target))
  
  # --------------make ascending
  test <- test %>% arrange(date)
  train <-train %>% arrange(date)
  trainCLEAN <- trainCLEAN %>% arrange(date)
  
  #---------------time slice cross validation
  train_n = trainCLEAN %>% nrow
  myTimeControl <- trainControl(method = "timeslice",initialWindow = window ,horizon = horizon,fixedWindow = TRUE,allowParallel = T,classProbs = TRUE)
  trControlNone = trainControl(method = "none",allowParallel = TRUE,classProbs = TRUE)
  trControlBoot = trainControl(method = "boot",allowParallel = TRUE,classProbs = TRUE)
  timeSlices <- createTimeSlices(1:nrow(train),  initialWindow = window, horizon = horizon, fixedWindow = TRUE)
  myTimeControl$index= timeSlices$train
  myTimeControl$indexOut= timeSlices$test
  folds=length(myTimeControl$index)
  myTimeControl$savePredictions=TRUE
  set.seed(12345)
  
  #fix zero var
  #nzv <- nearZeroVar(train, saveMetrics = TRUE) %>% subset(.,zeroVar==T) %>% rownames
  #train[,nzv]=train[,nzv] %>% sapply(function(x) sample(c(x-0.00001,x,x+0.00001), length(x), replace=TRUE)  )
  
  #-------glmnet elastic net: mix of L1 Lasso and L2 regularization
  mtype="glmnet"
  mlmodel_glmnet <- train(trainCLEAN[,features], trainCLEAN[,"target"],method=mtype,trControl=myTimeControl
                   ,preProcess=c("center","scale","pca")
                   ,metric = "Accuracy"
                   ,tuneGrid=tuneGrids[[mtype]]
  )
  print(mlmodel_glmnet)
  up <- predict(object=mlmodel_glmnet, test[,features], type='prob')[,"up_change"]
  down <- predict(object=mlmodel_glmnet, test[,features], type='prob')[,"down_stay"]
  probs_glmnet=up/(up+down)
  
  is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
  probs = probs_glmnet 

  #--------------final
  t=test[,"date",drop=FALSE]
  t$probs = probs
  result = rbind(result,t)
}

#-----save predictions
NAME_MODEL = paste(c(targetSymbol,"R","glment",train.days,test.days,train_threshold), collapse = "_")
write.csv(result,"models_prediction/" %+% NAME_MODEL %+% ".csv",row.names=F)

#-----save model and features
saveRDS(mlmodel_glmnet,"models/" %+% NAME_MODEL %+% ".rds")
saveRDS(features,"models/" %+% NAME_MODEL %+% "_features.rds")



#----cross.cor (ordered ASC: cross.cor.lag > 0 means past data ------# ASC: lag moves past data down to future, lead moves future data up to past
# sf = sf %>% arrange(desc(date))
# #Find_Max_CCF(sf$f.tec_roc.usd.Close,sf$target_bin,sf$date)
# d=sf[(sf$target_delta %>% abs) > 0.1 ,]
# ccor=lapply(d[,names(d) %like% "f."],Find_Max_CCF,d$target_1.pure,d$date) 
# ccor=lapply(names(ccor),function(x) {
#   ccor[[x]]$var = x
#   ccor[[x]]
# }) %>% do.call("rbind",.) %>% arrange(desc(abs(cor)))
# ccor %>% filter(lag %in% c(0))
    
    






#-------Example file to evaluate models and calculate returns with fees

source("functions_kpis.R")


#--------load
MODELS = list.files("models_prediction")
MODEL = MODELS[1]
TARGET = strsplit(MODEL,"_")[[1]][1]
d=load_predictions(MODEL)
d %>% head(5)

#-----CUTOFF KPIS
KPIS_CUTOFF = get_kpis_cutoffs(d)
KPIS = list()
MONEY = list()

for (cu in KPIS_CUTOFF$cutoff) {
  
  m=create_returns(d,INVEST=1000,cutoff=cu,FEE_LONG=0.0001,FEE_SHORT=0.0001,FEE_FIX=1) 
  MONEY[[as.character(round(cu,2))]] = m$returns
  ret = table.AnnualizedReturns(as.xts(m)$returns_delta, scale = NA, Rf = 0.0002, geometric = TRUE, digits = 2)
  roi_total = m$returns %>% tail(1) / m$returns[1]
  roi = data.frame(returns_delta=roi_total,row.names = "ROI total" )
  k=rbind(ret,roi)
  names(k) = round(cu,2)
  KPIS[[length(KPIS)+1]] =k
  
}

KPIS = do.call("cbind",KPIS)
MONEY = data.frame(do.call("cbind",MONEY))
MONEY$date = as.Date(rownames(m))

print(KPIS)

#----functions add fuutre
import_investing.com = function(path,name) {
  ff = read.csv(path)[,1:6]
  ff = ff[1:(nrow(ff)-2),]
  names(ff) = c("date",".Close",".Open",".High",".Low",".Volume")
  names(ff)[2:6] = name %+% names(ff)[2:6]
  ff = apply(ff, 2, gsub, patt=",", replace="") %>% as.data.frame
  ff[,6]=sapply(ff[,6], function(x) {
    if ( grepl("M",x) ) return(as.numeric(gsub("M","",x)) * 1000000)
    if ( grepl("K",x) ) return(as.numeric(gsub("K","",x)) * 1000) else return(x %>% as.numeric)
  }) %>% unlist
  ff[,2:6] = apply(ff[,2:6],2,as.numeric)
  ff$date = as.Date(ff$date, "%b %d %Y")
  ff
}

#------------EOD data YAHOO quantmod
download=foreach (symb=STOCKS_DOWNLOAD,.errorhandling = "remove") %do% {
  print(symb)
  s=getSymbols(symb, src = "yahoo",auto.assign = F,from="2006-01-01")
  s=s[as.Date(index(s))  >= as.Date("2006-01-01")]
  tail(s) %>% print
  s
}

#--------------cleaning symbols
draw = download
names(draw) = STOCKS_DOWNLOAD
names(draw) = gsub("\\^","",names(draw))
names(draw) = gsub("000001.SS","SS",names(draw))
names(draw$SS) = gsub("000001.SS","SS",names(draw$SS))
  
#------------combine for date
draw=draw[!is.null(draw)]
draw = draw %>% lapply(function(x) {
  datex = index(x) %>% as.Date
  x = x %>% as.data.frame
  x$date = datex
  x
})

#------------additional PCR COBE
pcr_index = read.csv("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/indexpc.csv",skip = 2)
names(pcr_index) = c("date","f.pcr_index_call","f.pcr_index_put","f.pcr_index_total","f.pcr_index_pcr")
pcr_index$date = as.Date(strptime(pcr_index$date,format="%m/%d/%Y"))

pcr_tot = read.csv("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/totalpc.csv",skip = 2)
names(pcr_tot) = c("date","f.pcr_tot_call","f.pcr_tot_put","f.pcr_tot_total","f.pcr_tot_pcr")
pcr_tot$date = as.Date(strptime(pcr_tot$date,format="%m/%d/%Y"))

draw$pcr_index = pcr_index
draw$pcr_tot = pcr_tot

#------------additional Futures from investint.com (requires manual download!)
if (ADD_FUTURES_INVESTING_COM == T) {
    draw$FTSE_100_FU = import_investing.com("stocks_day/FTSE 100 Futures Historical Data.csv","FTSE_100_FU")
    draw$FTSE_250_FU = import_investing.com("stocks_day/FTSE 250 Futures Historical Data.csv","FTSE_250_FU")
}


saveRDS(draw,"stocks_day/stocks_list.rds")

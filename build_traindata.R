Sys.setenv(TZ='America/New_York')
"%+%" <- function(x,y) {paste(x,y,sep="")}
source("functions_general.R")
source("functions_features.R")
source("functions_eval.R")

# http://www.qmatix.com/XLQSymbolGuide.htm
#stockpair = unique(as.vector(read.table("http://algotrade.glueckert.net/public/stockpair_symbs.txt")$V1))
#all_stocks = as.vector(read.table("http://algotrade.glueckert.net/public/allstock_symbs.txt")$V1)


#--------SET symobl or set of symbols to create training data
targetSymbol=c("FTSE") 


#--------Download
STOCKS_DOWNLOAD = c("AUD=X","EUR=X","JPY=X","OIL",
                    "^AXJO","^N225",
                    "^VIX","^GDAXI","^GSPC","^DJI","FXI","000001.SS",
                    "^FTSE")

EUROPE = c("FTSE","FTSE_350","FTSE_FU","GDAXI","FTSE_100_FU","FTSE_350","SS","FTSE_250_FU","HSBA","FTLC")
ADD_FUTURES_INVESTING_COM = F
STOCK_SHIFT = c("AXJO", "N225","SS")

#-------Build
source("build_stocks_day_raw.R")
source("build_stocks.R")
source("build_features_day.R")

#getSymbols("^AXJO", src = "yahoo",auto.assign = F,from="2018-01-01") %>% tail

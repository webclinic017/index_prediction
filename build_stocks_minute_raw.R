setwd("/var/www/vhosts/algotrade.glueckert.net/httpdocs/binary_options")
source("functions_general.R")
source("../algotrade/functions_dailyquotes.R")

# http://www.qmatix.com/XLQSymbolGuide.htm

stockpair = unique(as.vector(read.table("http://algotrade.glueckert.net/public/stockpair_symbs.txt")$V1))
all_stocks = as.vector(read.table("http://algotrade.glueckert.net/public/allstock_symbs.txt")$V1)
stock_add = c("EUR=X","JPY=X","OIL","^AXJO","^N225", "^VIX","^FTSE","^GDAXI","^GSPC","^DJI")
all_symbs = unique(c(stockpair,stock_add)) 

#-----------SETTINGS
years = c(2010,2017)
symbs = sp
windows = c(5,15,60)
y0=min(years) %+% "-01-01"
y1=max(years) %+% "-12-31"
#-------------------

#------------minute data
all_symbs = c("JPY=X","TWTR","TSLA")
for (symb in all_symbs) {
  min_data=api_stock_rates(symb,y0,y1,historyType=0,intradayMinutes=1,prefix=symb%+%".",varnames="",batch=c(y0,y1,30))
  saveRDS(min_data,"stocks_minute/" %+% symb %+% ".rds")
  print(symb %+% "")
}


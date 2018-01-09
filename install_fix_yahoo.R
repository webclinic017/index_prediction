# fix R packages so that yahoo works
install.packages("zoo")
install.packages("xts")
install.packages("TTR")
remove.packages("quantmod")
devtools::install_github("joshuaulrich/quantmod")

library(quantmod)

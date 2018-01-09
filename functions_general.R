#-----needed packages
library(tidyverse)
library(magrittr)
library(data.table)
library(quantmod)
library(foreach)
library(doParallel)
library(Hmisc)
library(ROCR)
library(caret)

"%+%" <- function(x,y) {paste(x,y,sep="")}
"%g%" <- function(x,y) {x[grepl(y,x)]}
"%gp%" <- function(x,y) {x[grepl(y,x,perl=TRUE)]}

sprint = function(x) {
  v = ""
  for (e in x) v=v %+% "-" %+% e
  v
}


get_name = function(v){
  strsplit(v,"\\.") %>% lapply(.,head,1) %>% unlist
  gsub(".Close","",v)
}

rev_me = function(x) {
  if (is(x)[1] == "data.frame") 
    return(x[rev(1:nrow(x)),])
  else if (is(x)[1] == "vector") 
    return(rev(x))
  else
    return(x)
}

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}



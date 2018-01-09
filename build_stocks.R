#--------------select select stocks list
draw = readRDS("stocks_day/stocks_list.rds")
names(draw)

#------shift dates for special stocks (timezone)
# if target is European
if (targetSymbol %in% EUROPE) {
    
    for (s in STOCK_SHIFT) {
    maxDate = draw[[s]] %>% subset(date == max(date))
    maxDate[,names(maxDate) != "date"] = NA
    draw[[s]]$date = draw[[s]]$date -1
    sat = draw[[s]]$date %>% weekdays == "Sunday"
    draw[[s]]$date[sat] = draw[[s]]$date[sat]-2
    draw[[s]]= plyr::rbind.fill( draw[[s]],maxDate)
    }
}

# SHIFT next day of GDAXI and FTSE as predictor (Time ZONE!)
if (!targetSymbol %in% EUROPE) {
    # then stock prediction starting from 12:00 AM!
}

#------combine
d1 = draw[sapply(draw,nrow) %>% unlist > 900]
d1 = Reduce(function(x, y) merge(x, y, all=T,by.x="date", by.y="date"), d1, accumulate=F)
names(d1) = gsub("\\^","",names(d1))

#------normalize dates: use date of target
d2 = d1[!is.na(d1[,targetSymbol %+% ".Close"]),]
d2 = d2[!duplicated(d2$date),]
d2 %>% tail(5)

#-----replace 0 Volume with NA
d2[d2 == 0] <- NA

# remove 0 VAR and NAs
nzv <- nearZeroVar(d2, saveMetrics = TRUE)
nzv.out = rownames(subset(nzv,nzv==TRUE))
NAs.col = apply(d2,2,function(x) {length(which(is.na(x))) / length(x)})
NAs.out = NAs.col[NAs.col > 0.3] %>% names
vars.out =  c(nzv.out,NAs.out)
d3 = d2[,!(names(d2) %in% vars.out)]

saveRDS(d3,"stocks_day/stocks_combined_" %+% targetSymbol %+% ".rds")

#---Check stock splits
ggplot() + geom_line(data = d3, aes(x = date, y = eval(parse(text=targetSymbol %+%  ".Open"))), color = "blue")  + 
            geom_line(data = d3, aes(x = date, y = eval(parse(text=targetSymbol %+% ".Close"))), color = "red") 
      

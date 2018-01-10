library(ggplot2)
library(scales)
library(reshape2)
library(quantmod)
library(markdown)
library(DT)
library(caret)
library(magrittr)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(plotly)
library(PerformanceAnalytics)
library(ROCR)
library(e1071)


#---------------------FUNCTIONS
source("functions_general.R")
source("functions_kpis.R")

find_top = function(df,top) {
  if ((nrow(df)) > (top) )
    return(df=rbind(df %>% head(top/2),df %>% tail(top/2)))
  else
    return(df)
}

scale_end = function(t,to) {
  from=to[1]
  to=to[2]
  fact = to - t[length(t)]
  fs = seq(0,fact,length.out = length(t))
  t=t+fs
  t
}

calc_win = function(alerts,tradesize,bin_win,bin_loss,hit) {
  trades_win = trunc(alerts*hit)
  trades_loss = alerts - trades_win 
  win=trades_win * tradesize * bin_win
  loss=trades_loss * tradesize * bin_loss
  out = round(win - loss,0)
  return(out)
}

create_kpis=function(invested,rate_return,DAYS_TRADING_YEAR) {
  out=list()
  multiple = round(1+rate_return/100,2)
  total=round(invested*multiple,0)
  profit=total-invested
  profit_day=profit/DAYS_TRADING_YEAR
  profit_month=round(profit/12,0)
  out=list(invested=invested,total=total,profit=profit,multiple=multiple,profit_day=profit_day,
           profit_month=profit_month)
  out
}

"%+%" <- function(x,y) {paste(x,y,sep="")}
rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
"%g%" <- function(x,y) {x[grepl(y,x)]}
"%gp%" <- function(x,y) {x[grepl(y,x,perl=TRUE)]}



#-------------------------STATIC VARIABLES
font = list( size = 10, color = 'blue')

# http://www.barclayhedge.com/research/indices/ghs/Hedge_Fund_Index.html
DAYS_TRADING_YEAR = 250
SP5 = read.csv("results/benchmark_GSPC_.csv")

#-------------------JAVASCRIPT

function(input, output, session) {
  
#-------------------wanrnings
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1) 

#-------------reactive functions----
  
observeEvent(input$BUT_startsim, {
  toggleModal(session, "MODAL_sim", toggle = "close")
  updateSelectInput(session, "i_trade", selected = input$i_modal_invest)
})
  
  
list_invest =  reactive({ 
  
  withProgress(message = 'Calculation in progress', detail = '...', value = 50, {
      #---binary profit
      out = list()
      tradesize = input$i_trade
      alert_day = input$i_alert_day
      alerts = alert_day * DAYS_TRADING_YEAR
      hit = input$i_hit/100
      bin_win = input$i_bin_win/100
      bin_loss = input$i_bin_loss/100
      invested=alerts*tradesize
      profit=calc_win(alerts,tradesize,bin_win,bin_loss,hit)
      total=invested+profit
      multiple=round(total/invested,3)
      profit_day=round(profit/DAYS_TRADING_YEAR,2)
      profit_month=round(profit/12,1)
      out$kpis = list(invested=invested,total=total,profit=profit,multiple=multiple,profit_day=profit_day,
               profit_month=profit_month)
      
      #----hedge fund and sp500
      out$sp500 = create_kpis(invested,input$i_sp500,DAYS_TRADING_YEAR)
      out$hedge = create_kpis(invested,input$i_hedge,DAYS_TRADING_YEAR)
      
      #---plot
      
      o=data.frame(hit = rbinom(DAYS_TRADING_YEAR,1,input$i_hit/100))
      o$money = -input$i_trade * input$i_bin_loss/100
      o[o$hit == 1,]$money = input$i_trade * input$i_bin_win/100
      o=o[sample(1:nrow(o)),]
      o$hit=NULL
      o$money_cum=cumsum(o$money)
      o$money_cum = o$money_cum + invested
      o$binary = o$money_cum/o$money_cum[1]
      
      o$binary = scale_end(o$binary, to=c(1,out$kpis$multiple))
      o$sp500 = scale_end(SP5$delta, to=c(1,out$sp500$multiple))
      o$hedge = scale_end(SP5$delta2, to=c(1,out$hedge$multiple))
      o$day = c(1:DAYS_TRADING_YEAR)
      out$sim_plot = o
      out
      
  })
})


index_pred = reactive({
  
  withProgress(message = 'Calculation in progress', detail = '...', value = 50, {
  
      MODEL = input$i_model
      TARGET = strsplit(MODEL,"_")[[1]][1]
      d=load_predictions(MODEL)
      
      #-----CUTOFF KPIS
      KPIS_CUTOFF = get_kpis_cutoffs(d)
      KPIS = list()
      MONEY = list()
      
      for (cu in KPIS_CUTOFF$cutoff) {
        
        m=create_returns(d,INVEST=input$i_indexinvest,cutoff=cu,FEE_LONG=input$i_feelong/100,FEE_SHORT=input$i_feeshort/100,FEE_FIX=input$i_feefix) 
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

  })
  
  list(kpis=KPIS,money=MONEY,cuts=KPIS_CUTOFF,target=TARGET)
})


#--------------Outputs
  output$k_invested2 <- renderText({"Invest: $" %+% list_invest()$kpis$invested})
  output$k_invested <- renderText({list_invest()$kpis$invested})
  output$k_total <- renderText({list_invest()$kpis$total})
  output$k_profit <- renderText({list_invest()$kpis$profit})
  output$k_multiple <- renderText({ (list_invest()$kpis$multiple*100-100) %+% "%"})
  output$k_profit_month <- renderText({list_invest()$kpis$profit_month})
  
  output$sp_invested <- renderText({list_invest()$sp500$invested})
  output$sp_total <- renderText({list_invest()$sp500$total})
  output$sp_profit <- renderText({list_invest()$sp500$profit})
  output$sp_multiple <- renderText({ (list_invest()$sp500$multiple*100-100) %+% "%"})
  output$sp_profit_month <- renderText({list_invest()$sp500$profit_month})

  output$he_invested <- renderText({list_invest()$hedge$invested})
  output$he_total <- renderText({list_invest()$hedge$total})
  output$he_profit <- renderText({list_invest()$hedge$profit})
  output$he_multiple <- renderText({ (list_invest()$hedge$multiple*100-100) %+% "%"})
  output$he_profit_month <- renderText({list_invest()$hedge$profit_month})

  
#--------------Rendering
  
  output$example_plot <- renderPlotly({
    db=list_invest()$sim_plot
    db %>% plot_ly(x = ~day, y = ~ binary, name="binary", mode = "lines", type = "scatter") %>% 
      add_trace(y = ~sp500, name = "sp500", mode = "lines") %>% 
      add_trace(y = ~hedge, name = "hedge fund index", mode = "lines") %>%  layout(title = "Binary option returns benchmark" ) 
    #ggplot(db, aes(x=day)) + geom_line(aes(y = binary, colour = "binary")) + geom_line(aes(y = sp500, colour = "sp500"))+ geom_line(aes(y = hedge, colour = "hedge funds"))
  })
  

  
  
  output$index_kpis <- renderPrint({
      index_pred()$kpis
  })
  
  output$index_cuts <- renderPrint({
    index_pred()$cuts
  })
  
  output$table_benchmark <- DT::renderDataTable({
    DT::datatable(
      read.table("results/table_benchmark.csv",header=T),rownames= FALSE
    )
  })

  output$cfd<- renderPlotly({
    
          ip = index_pred()
          best = round(max(ip$kpis["ROI total",]),2)
          cfd_title= ip$target %+% " cumultative returns by probability CUTOFF\nbest ROI : " %+% best %+% "x"
          dp=melt(ip$money,id.vars = "date")
          #ggplot(dp, aes(x=date)) + geom_line(aes(y = value, colour = variable))
          plot_ly(dp,x=~date,y=~value, type='scatter',mode='lines',split=~variable)  %>%  layout(title = cfd_title )
  
  })
  
  output$table_probs <- DT::renderDataTable({
    DT::datatable(
      cars,rownames= T
    )
  })
 
}

#

# result05 %>% plot_ly(x = ~date, y = ~ X0.5, name="CFD_0.5", mode = "lines", type = "scatter") %>% 
#   add_trace(y = ~X0.6, name = "cutoff 0.6", mode = "lines") %>% 
#   add_trace(y = ~X0.7, name = "cutoff 0.7", mode = "lines") %>% 
#   layout(font=font,title = cfd_title ) 

# output$index_kpis <- DT::renderDataTable({
#   DT::datatable(
#     index_pred()$kpis,rownames= FALSE,options = list(pageLength = 5)
#   )
# })

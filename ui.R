library(shinyBS)
library(plotly)
library(pROC)
library(PerformanceAnalytics)

"%+%" <- function(x,y) {paste(x,y,sep="")}
stock_names = readRDS("stock_names.rds")

fluidPage(
  tags$head(tags$script(src="nav.js")),
  shinyjs::useShinyjs(),
  includeCSS("custom.css"),
  navbarPage("MoneyPrinter 0.4",
             tabPanel("Algo-Trading",class="landing",icon = icon("info-circle",lib = "font-awesome"),
                      HTML("<div class='pcont'>
                           <span class='glyphicon glyphicon-education iconbig'></span>
                           <h2>Algotrading with machine learning</h2>
                           predict stock or index direction using machine learning<br>
                           trade as <a target='_blank' href='http://www.investopedia.com/terms/b/binary-option.asp'>binary option</a> on <a target='_blank' href='http://www.stockpair.com'>Stockpair</a><br>
                           trade indices on <a target='_blank'  href='http://www.interactivebrokers.com'>Interactive Brokers</a>
                          
                           </div>"),
                      tags$div(class="pcont",
                               actionButton("BUT_learn", "About Binary Options",class="btn btn-primary"),
                               actionButton("BUT_sim", "Binary Option Simulation",class="btn btn-primary"),
                               actionButton("BUT_res", "Index Prediction",class="btn btn-primary")
                               #,actionButton("BUT_git", "Github",class="btn btn-primary", icon = icon("fa-github",lib = "font-awesome"),onclick = "window.open('https://github.com/KlausGlueckert/binary_options', '_blank')" )
                               ),
                      
                      
                      HTML("<div class='pcont'> <img class='big' src='bill.jpg'></div>"),
                      
  
                                      
                                      bsModal("MODAL_sim", "Simulate returns", "BUT_sim", size = "small",
                                              tags$p("How much would you like to invest?"),
                                              selectInput("i_modal_invest", "Invest $:", choices = list("5000" = 20, "10000" = 40,"25000" = 200), selected = 20), 
                                              actionButton("BUT_startsim", "Simluate",class="btn btn-primary")
                                      ),
                      
                                      bsModal("MODAL_learn", "Learn about binary options", "BUT_learn", size = "large",
                                              HTML("<div class='pcont'>
                                           <span class='glyphicon glyphicon-piggy-bank iconbig'></span>
                                                   <h2>What are binary options?</h2>
                                                   A <a href='http://www.investopedia.com/articles/active-trading/061114/guide-trading-binary-options-us.asp'>binary option</a> is a trading instrument 
                                                   where one bets money on the direction of a stock (or currency, index) for a fixed time intervall. 
                                                   In a simple form, the trader can profit a margin up to 100% per trade when correct, when incorrect looses up to
                                                   a maximum of 100% of the trade (but never  more). There are regulated US-based and European-based binary options working
                                                   slightly differently.
                                                   </div>"),
                                      HTML("<div class='pcont'> <img src='sp.jpg'></div>"),
                                      HTML("<div class='pcont'>
                                           <span class='glyphicon glyphicon-cloud-upload iconbig'></span>
                                           <h2>Trading platform?</h2>
                                           There are several regulated and unregulated trading platforms such as 
                                           <a href='http://www.stockpair.com'>Stockpair (EU)</a> and <a href='http://www.nadex.com'>Nadex (USA)</a>. 
                                           Some even offer <a href='https://www.stockpair.com/dev/trading-api'>trading APIs</a> for free. 
                                           One can start trading by loading an account with a few 100 dollar after a KYC check.
                                           </div>"),
                                      HTML("<div class='pcont'>
                                           <span class='glyphicon glyphicon-info-sign iconbig'></span>
                                           <h2>Whats special about binary options?</h2>
                                           <div style='text-align:left'>
                                           <ul>
                                           <li>easy to understand</li>
                                           <li>upside and downside are defined (no market risk)</li>
                                           <li>no extra or minimal trading fees</li>
                                           <li>no security deposits due to hedging risk</li>
                                           <li>tradable every second</li>
                                           <li>trading APIs, no expensive tools like <a href='https://www.interactivebrokers.com/en/home.php'>interactive brokers</a></li>
                                           <li>human trading on 'gambling' platforms, good for pattern recognition</li>
                                           <li>in Europe: no day trading licence</li>
                                           </ul></div>
                                           </div>"),
                                      HTML("<div class='pcont'>
                                           <span class='glyphicon glyphicon-info-sign iconbig'></span>
                                           <h2>US vs. Europe style binary options?</h2>
                                           European style binary options just let you 'gamble' on the direction of a stock with a fixed win-rate and loss-rate. 
                                            For example, on binary option platforms you bet $100 dollars on that Apple stock will go up in the next our. 
                                            If you are correct you receive 80% profit, you incorrect you loose your money. The win-loss ratio is asymetrical,
                                            therefore you have to be better than random to trade profitably. US style binary options work with bid-ask spread and
                                             are modelled after regular option trading. See this <a href='http://www.investopedia.com/articles/active-trading/061114/guide-trading-binary-options-us.asp'>investopedia article</a>.
                                            
                                           </div>")
                                              
                                      )
                      
                      ),
             
             tabPanel("Binary Option Simulation",id="tab_simulation",icon = icon("cubes",lib = "font-awesome"),
                      sidebarLayout(
                        sidebarPanel(
                          tags$label(class="boxhead","Settings binary:"),tags$hr(),
                          sliderInput("i_trade", "Trade size ($):", min = 20, max = 200, value = 20, step = 20,pre = "$", sep = ","),
                          
                          sliderInput("i_alert_day", "Trades per day:",min = 1, max = 10, value = 1, step = 1,sep = ","),
                          
                          sliderInput("i_hit", "Model Accuracy or hit rate:", min = 50, max = 90, value = 70, step = 0.2, pre = "%", sep = ","
                                      ,animate=animationOptions(interval=3000, loop=F)
                                      ),
                          
                          sliderInput("i_bin_win", "Per trade return, direction right:",min = 70, max = 100, value = 80, step = 10,pre = "%", sep = ","),
                          
                          sliderInput("i_bin_loss", "Per trade loss, direction wrong:",min = 70, max = 100, value = 100, step = 10,pre = "%", sep = ","),
                          
                          tags$hr(),
                          
                          
                          tags$label(class="boxhead","Settings benchmark:"),tags$hr(),
                          
                          sliderInput("i_sp500", "Yearly return S&P 500:",min = 7, max = 7, value = 7, step = 1, pre = "%", sep = ","),
                          
                          sliderInput("i_hedge", "Yearly return hedge fund:",min = 11, max = 11, value = 11, step = 1, pre = "%", sep = ",")
                        ,width=2),
                        
                        mainPanel(
                          
                          
                          
                          tags$div(class = "well plot",
                                   tags$label(class="boxhead","Simulated return 1 year: "),tags$hr(),
                                   fluidRow( column(2,HTML("<div class='kpi_head kpi'>Type</div>") ), 
                                             column(2,HTML("<div class='kpi_head kpi'>%Return</div>")),
                                             column(2,HTML("<div class='kpi_head kpi'>$ Invested</div>")),
                                             column(2,HTML("<div class='kpi_head kpi'>$ Total</div>")),
                                             column(2,HTML("<div class='kpi_head kpi'>$ Profit</div>")),
                                             column(2,HTML("<div class='kpi_head kpi'>$ Profit Month</div>"))
                                   ),
                                   fluidRow( column(2,tags$div(class = "kpi","Binary option" )),   
                                             column(2,tags$div(class = "well kpi_box kpi",id="BIN_MULTIPLE",textOutput("k_multiple"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("k_invested"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("k_total"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("k_profit"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("k_profit_month"))  )
                                             
                                   ),
                                   fluidRow( column(2,tags$div(class = "kpi","S&P500" )),   
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("sp_multiple"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("sp_invested"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("sp_total"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("sp_profit"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("sp_profit_month"))  )
                                             
                                   ),
                                   fluidRow( column(2,tags$div(class = "kpi","Hedge Fund Index" )),   
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("he_multiple"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("he_invested"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("he_total"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("he_profit"))  ),
                                             column(2,tags$div(class = "well kpi_box kpi",textOutput("he_profit_month"))  )
                                             
                                   )
                          ),
                          tags$div(class = "well plot",
                                   tags$label(class="boxhead","Profit 1 year (250 trading days): "),tags$hr(),
                                   plotlyOutput("example_plot")
                          )
                        )
                      )
             )
             ,
                   tabPanel("Index Prediction",icon = icon("gear",lib = "font-awesome"),
                            
                      sidebarLayout(  
                        
                            sidebarPanel(
                              selectInput("i_model", "Select model: ", choices=list.files("models_prediction"), selected = 1, multiple = FALSE,selectize = FALSE),
                                sliderInput("i_indexinvest", "Investment ($):", min = 1000, max = 50000, value = 10000, step = 1000,pre = "$", sep = ","),
                                numericInput("i_feelong", "Fee long position (%): ", 0.01, min = NA, max = NA, step = 0.001),
                                numericInput("i_feeshort", "Fee short position (%): ", 0.01, min = NA, max = NA, step = 0.001),
                                numericInput("i_feefix", "Fee fix ($): ", 1.00, min = NA, max = NA, step = 1)
                                , width = 3),
                              
                            mainPanel(
                     
                                  tags$div(class = "well plot",
                                           tags$label(class="boxhead","Index day trade: long or short at market opening, re-invest daily"),tags$hr(),
                                           plotlyOutput("cfd"))
                                  ,
                                  tags$div(class = "well plot",
                                           tags$label(class="boxhead","Performance by probability cutoff"),tags$hr(),
          
                                           verbatimTextOutput("index_kpis"),
                                           verbatimTextOutput("index_cuts")
                                           
                                  ),
                                  tags$div(class = "well plot",
                                           tags$label(class="boxhead","Data"),tags$hr(),
                                           tags$p("later")
                                  )
                                  )
                            
                            ,fluid=F)
             )
             
             ,
             tabPanel("Live Trading",icon = icon("tachometer",lib = "font-awesome"),
                      HTML("coming soon...")
             )
                      )
                      )
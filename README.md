# Dataincubator Capstone project: directional index prediction
===============================================================================

* **Goal**: predict daily stock index direction using machine learning and explore binary option trading and live trading on interactive brokers

* **Result**: https://glueckert.shinyapps.io/index_prediction/

The webapp shows:

* explantion and resources regarding binary options
* example simulation for binary option trading
* backetesting results for stock index prediction (mostly FTSE) using machine learning in R and Python

Data used

* for end-of-day data: yahoo finance via [quantmod package](https://cran.r-project.org/web/packages/quantmod/quantmod.pdf)
* for minute data: [activetick API](http://wwww.activetick.com)

Key files
===============================================================================
`/build_traindata.R`
builds training data for index defined in file

`/predict_nextday.R`
prediction model for next day prediction of direction using R

`/build_kpis.R`
Example file to evaluate models and calculate returns with fees

`/functions[...].R`
functions for building and modelling

Key directories
===============================================================================
`/data_training`
stores training data per stock (including features)

`/data_training`
stores test data per stock (target variables)

`/models_prediction`
stores predictions per index: format is [date, probability_price_up]. Webapp will parse folder present models as dropdown for evaluation

Tech stack
===============================================================================
Shiny-R application deployed to <a href="http://www.shinyapps.io/" target="_blank">shinyapps.io</a>

Analysis and machine learning
  * R
  * Python

Web & Graphics
  * R Shiny
  * Bootstrap
  * Custom CSS, HTML tweaks
  * JQuery / Javascript

Sources 
===============================================================================
Stock trading papers
* Global market approach for indeces: http://cs229.stanford.edu/proj2012/ShenJiangZhang-StockMarketForecastingusingMachineLearningAlgorithms.pdf
* https://pdfs.semanticscholar.org/4ecc/55e1c3ff1cee41f21e5b0a3b22c58d04c9d6.pdf
* NIKEI prediction with technical features, 0.8 ACC, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4873195/#pone.0155133.ref010 

Trading tutorials
* https://www.toptal.com/machine-learning/s-p-500-automated-trading
* http://francescopochetti.com/stock-market-prediction-part-ii-feature-generation/

Live data feeds
* https://www.quora.com/What-are-some-good-APIs-to-get-real-time-stock-quotes
* https://www.programmableweb.com/news/96-stocks-apis-bloomberg-nasdaq-and-etrade/2013/05/22
* http://quant.stackexchange.com/questions/32336/api-or-service-to-get-german-dax-and-uk-ftse-historical-and-or-live-minute-data
* http://quant.stackexchange.com/questions/18215/where-to-get-long-time-historical-intraday-data?rq=1

About binary options
* http://www.investopedia.com/articles/active-trading/061114/guide-trading-binary-options-us.asp
* http://www.investopedia.com/terms/b/binary-option.asp

Stock symbol guides:
* http://www.qmatix.com/XLQSymbolGuide.htm
=======

#' Author: Ted Kwartler
#' Date: Mar 6, 2023
#' Purpose: Simple Moving Avg Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)

# Grab historical data
stk   <- getSymbols("NVDA", auto.assign = F) 
stk   <- stk['2020-01-01/']
ma50  <- SMA(Cl(stk), 50)
ma200 <- SMA(Cl(stk), 200)

# Set up the indicator
tradingIndicator <- ifelse(ma50 > ma200, 1, 0)
tradeSignal <- Lag(tradingIndicator)

# Examine what we made
tradeSignal[199:205]

# How many trading days would we have money tied up vs not?
table(tradeSignal)

# Calc the rate of change (return)
#Rate of Change TTR::ROC(); not receiever operating curve from ML
ret <- ROC(Cl(stk))*tradeSignal 

# Review your return
charts.PerformanceSummary(ret)

# Compare to buy & hold
plot(Cl(stk))

# Now let's be knight cap and switch the logic!
tradeSignal <- Lag(ifelse(ma50 > ma200, 0, 1), k = 1)
ret         <- ROC(Cl(stk))*tradeSignal #Rate of Change TTR::ROC()

# Review your return
charts.PerformanceSummary(ret)

# End

#' Author: Ted Kwartler
#' Date: Mar 6, 2023
#' Purpose: Stock API request & Manipulate a Time Series Object
#' 

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(dygraphs)
library(htmltools)

## Get historical stock pricing
getSymbols("AAPL", src = "yahoo", auto.assign = T) 
#RobjectName <- getSymbols('AAPL', auto.assign=F)

# Review
head(AAPL)

# Quick Viz
barChart(AAPL) 

## Subsetting XTS
#Get Jan3 to June 21 2018
dateToDate <- AAPL["2018-01-03/2018-06-21"] 
head(dateToDate)
tail(dateToDate)

# Extracting specific columns & simple deltas
# Closing column only
head(Cl(AAPL),5)

# Opening price column
head(Op(AAPL),5)

# High price for the session
head(Hi(AAPL),5)

# Low price for the session
head(Lo(AAPL),5)

# Volume for the session
head(Vo(AAPL),5)

# Simple deltas
head(OpCl(AAPL),5)
?Cl

# D3 Viz
dygraph(AAPL$AAPL.Close)  %>% dyRangeSelector()


# Example Candlestick
# https://www.investing.com/equities/apple-computer-inc-candlestick
candleAAPL <- AAPL[,1:4]
dygraph(candleAAPL) %>%
  dyCandlestick() %>% dyRangeSelector()

# End
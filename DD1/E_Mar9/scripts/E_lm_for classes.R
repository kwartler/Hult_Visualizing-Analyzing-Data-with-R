#' Author: Ted Kwartler
#' Date: 03-01-2020
#' Purpose: What happens when regression is applied to a binary outcome?
#' 

# libs
library(ggplot2)
library(dplyr)
library(tidyverse)

# wd
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Data
diamonds <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/diamonds2023.csv')
dropAmt <- tail(quantile(diamonds$priceClean, probs = seq(.1,.95, by = .05)), 1)
diamonds <- subset(diamonds, diamonds$priceClean<dropAmt)

# Convert to binary
diamonds$icedOut <- ifelse(diamonds$priceClean >= 11000,1, 0)
diamonds$price   <- NULL


# Remember this?
p <- ggplot(diamonds, aes(Carat, icedOut)) +geom_point(alpha=0.2)
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(icedOut ~ Carat + 0, diamonds)
coefficients(fit)

# Add out model predictions; does this look like a good fit?
p <- p + geom_abline(intercept =  0, 
                     slope     = coefficients(fit),
                     color='red')
p

# Suppose you *could* get a 12 carat diamond
hopeDiamond  <- data.frame(Carat = 16)
bigRock <- predict(fit, hopeDiamond)

# 1= Yes, the diamonds is more than $11k; 0 means no.  What does this mean?  We must have used the wrong method!
bigRock

# End


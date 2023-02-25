#' Author: Ted Kwartler
#' Date: 2-19-2023
#' Purpose: Toyota Corolla Regression
#' 

# Libs
library(vtreat)
library(dplyr)
library(ModelMetrics)

# Options
options(scipen=999)

# SetWD
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Data
#cars <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/oldCar.csv') #old version
cars <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/newCars.csv') # new version

# drop geo and text for this example
cars$state       <- NULL
cars$city        <- NULL
cars$allFeatures <- NULL

# Partitioning 20% test set
splitPercent <- round(nrow(cars) %*% .8)

set.seed(2017)
idx      <- sample(1:nrow(cars), splitPercent)
trainSet <- cars[idx, ]
testSet  <- cars[-idx, ]

# EDA
summary(trainSet)

# Get the column names of our data frame
names(cars)

informativeFeatureNames <- names(cars)[6:25]
outcomeVariableName     <- names(cars)[26] # Or simply "listPrice"

# Preprocessing & Automated Engineering
# id & constant variable removal, dummy $Fuel_Type
dataPlan     <- designTreatmentsN(cars, 
                                  informativeFeatureNames, 
                                  outcomeVariableName)

treatedTrain <- prepare(dataPlan, trainSet)

#################
### Go to the ppt slide on Multi-Colinearity
#################

# Fit 3 variable model which has an multicolinearity engineered variable from vtreat 
# Don't worry about how vtreat created the variable...it's math.
fit <- lm(listPrice ~ fuelType_catP + fuelType_catN + fuelType_catD ,
          data = treatedTrain)

# Did R catch the issue?
summary(fit)

# Since we are comfortable with the data, we can concisely write a lm equation that includes all variables using period instead of an equation
fit <- lm(listPrice ~ ., treatedTrain)
summary(fit)

#################
### Go to the ppt slides on Summary Output
#################

# Drop uninformative vars
na.omit(coef(summary(fit)))
drops                 <- c('transmissionType_catD','fuelType_catN','transmissionType_catD',
                           'engineInfo_catD','style_catN','style_catD','priceClassDiffDirection_catD',
                           'ownersN_isBAD','vehicleHistUseType_lev_x_Frame_Damage',
                           'vehicleHistUseType_lev_x_Mixed_Use','vehicleHistUseType_lev_x_Personal_Use',
                           'vehicleHistUseType_lev_x_Rental_Use','transmissionType_lev_x_Automatic',
                           'fuelType_lev_x_Gas','fuelType_lev_x_Hybrid','driveType_lev_x_FWD',
                           'engineInfo_lev_x_1_8L_Inline_minus_4_Gas',
                           'engineInfo_lev_x_1_8L_Inline_minus_4_Hybrid',
                           'engineInfo_lev_x_2_0L_Inline_minus_4_Gas','style_lev_x_Sedan',
                           'priceClassDiffDirection_lev_NA','priceClassDiffDirection_lev_x_above',
                           'priceClassDiffDirection_lev_x_below','priceClassification_lev_x_Fair_Price',
                           'priceClassification_lev_x_Great_Price','priceClassification_lev_x_High_Price',
                           'mileageRatingCity_lev_x_29_cty_','mileageRatingCity_lev_x_30_cty_',
                           'mileageRatingCity_lev_x_31_cty_','mileageRatingCity_lev_x_53_cty_')
treatedTrainParsimony <- treatedTrain[, !(names(treatedTrain) %in% drops)]

fit2 <- lm(listPrice ~ ., treatedTrainParsimony)
summary(fit2)

#################
### Go to the ppt
#################

# Get Training Set Predictions
# Warning can be ignored but for those interested: 
# https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
trainingPreds <- predict(fit2, treatedTrainParsimony)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$listPrice,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$listPrice-trainingPreds )
head(trainingResults)

# What is the RMSE? 
# Be careful!  Different libraries have subtle differences, but shouldn't be an issue with RMSE but can cause issues with other KPI
# library(ModelMetrics) has rmse(a, p)
# library(MLmetrics) has RMSE(p, a)
(trainRMSE <- MLmetrics::RMSE(trainingResults$predicted, 
                              trainingResults$actuals))

# What is the MAPE?
(trainMAPE <- MLmetrics::MAPE(trainingResults$predicted, 
                              trainingResults$actuals))

# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation
testPreds <- predict(fit2, testSet)

# Oops!  
# We didn't prepare our data the EXACT same way as the training set and got an error that an expected variable is missing!!
treatedTest <- prepare(dataPlan, testSet)
testPreds   <- predict(fit2, treatedTest) 

#Organize training set preds
testResults <- data.frame(actuals   = testSet$listPrice,
                          predicted = testPreds)
head(testResults)

# KPI
(testRMSE <- MLmetrics::RMSE(testResults$predicted, 
                             testResults$actuals))

# What is the MAPE?
(testMAPE <- MLmetrics::MAPE(testResults$predicted, 
                             testResults$actuals))

# Side by Side
trainRMSE
testRMSE

trainMAPE
testMAPE

# End 


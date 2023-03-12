#' Author: Ted Kwartler
#' Data: Mar 12, 2023
#' Purpose: Load data build a random forest tree with ranger which is MUCH faster; this version uses more equally balanced target classes
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")
options(scipen=999)

## Load the libraries
library(MLmetrics)
library(ranger)
library(vtreat)
library(ggplot2)
library(ggthemes)

## Bring in some data
dat <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/F_Mar13/data/bank-downSampled.csv')

# EDA
names(dat)
head(dat)
summary(dat)

# Prep and non prep
set.seed(2022)
idxPrep        <- sample(1:nrow(dat),.1*nrow(dat))
prepData    <- dat[idxPrep,]
nonPrepData <- dat[-idxPrep,]

# Treatment
targetVar       <- names(prepData)[17]
informativeVars <- names(prepData)[1:16]


# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar,'yes')

# Partition to avoid overfitting
set.seed(1234)
idx        <- sample(1:nrow(nonPrepData),.8*nrow(nonPrepData))
train      <- nonPrepData[idx,]
validation <- nonPrepData[-idx,]

# Now apply the variable treatment plan
treatedTrain <- prepare(plan, train)
treatedTest  <- prepare(plan, validation)

# Fit a random forest model with Ranger
# Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data.
moreVoters <- ranger(as.factor(Class) ~ .,
                     data  = treatedTrain, 
                     num.trees = 120,
                     importance = 'permutation',
                     mtry  = 1)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(moreVoters)),
                       importance = importance(moreVoters),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()


# Confusion Matrix
trainClass <- predict(moreVoters, treatedTrain)
confusionMatrix(trainClass$predictions, as.factor(treatedTrain$Class))

### Now let's apply to the validation test set
oneHundredTwentyVoters <- predict(moreVoters, treatedTest)

# Accuracy Comparison from MLmetrics
Accuracy(treatedTest$Class, oneHundredTwentyVoters$predictions)

# End

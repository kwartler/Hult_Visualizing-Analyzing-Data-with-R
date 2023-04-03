#' Author: Ted Kwartler
#' Data: Apr 3, 2023
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
library(caret)

## Bring in some data
dat <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/F_Mar13/data/bank-downSampled.csv')

# EDA
names(dat)
head(dat)
summary(dat)

# Duration is a target leaked variable
dat$duration <- NULL

# Prep and non prep
set.seed(2022)
idxPrep        <- sample(1:nrow(dat),.1*nrow(dat))
prepData    <- dat[idxPrep,]
nonPrepData <- dat[-idxPrep,]

# Treatment
targetVar       <- names(prepData)[16]
informativeVars <- names(prepData)[1:15]


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
                     mtry  = 1, 
                     probability = T)

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
classOutcome <- ifelse(trainClass$predictions[,2]>=0.5,'yes','no')
confusionMatrix(as.factor(classOutcome), 
                as.factor(treatedTrain$Class))

### Now let's apply to the validation test set
oneHundredTwentyVoters <- predict(moreVoters, treatedTest)

# Accuracy Comparison from MLmetrics
classOutcomeTest <- ifelse(oneHundredTwentyVoters$predictions[,2]>=0.5,
                           'yes','no')

Accuracy(as.factor(classOutcomeTest), 
         as.factor(treatedTest$Class))

## Another method that is fast but can help you identify the optimal number of trees is the ranger method using caret.  The only "gotcha" is starting each parameter with a .
grid <- expand.grid(.mtry = c(1,2),
                    .splitrule = 'extratrees',
                    .min.node.size = c(1,2))
fitControl <- trainControl(method = "CV",
                           number = 2,
                           verboseIter = TRUE)

fit <- train(as.factor(Class) ~ ., data = treatedTrain,
             method = 'ranger',
             num.trees = 200,
             tuneGrid = grid,
             trControl = fitControl)
fit$finalModel

# unfortunately you can't grid search trees so you can let caret decide 
fit <- train(as.factor(Class) ~ ., data = treatedTrain,
             method = 'ranger',
             tuneGrid = grid,
             trControl = fitControl)
fit$finalModel$num.trees
fit$finalModel$prediction.error

# I think its interesting to examine it as they grow but this takes a long time
numTreesVec <- vector()
oobError  <- vector()
nTreeSearch <- seq(from = 100, to = 500, by=5)
for(i in 1:length(nTreeSearch)){
  print(i)
  fit <- train(as.factor(Class) ~ ., data = treatedTrain,
               method = 'ranger',
               num.trees = nTreeSearch[i],
               tuneGrid = grid,
               trControl = fitControl)
  numTreesVec[i] <- fit$finalModel$num.trees
  oobError[i] <- fit$finalModel$prediction.error
}
results <- data.frame(ntrees =numTreesVec,
                      oobError = oobError)
ggplot(results, aes(x=ntrees,y=oobError)) + geom_line(alpha =0.25, color = 'red') +
  theme_gdocs()+
  geom_smooth(method = "loess")
# So here I would probably choose 250

# End

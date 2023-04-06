#' Author: Ted Kwartler
#' Date: Mar 20, 2023
#' Purpose: Biased Modeling Example
#'Megacorp is a hypothetical large and successful corporation that makes modern high-tech products. Whenever Megacorp advertises new job vacancies, their human resources team are overwhelmed by the many people who apply for a role. They want an automated process to filter through the resumes, to give them a short list of applicants who match best. Megacorp has a database containing the resumes and hiring results of applicants from the past few years. They track variables like age, gender, education and other details around the job applicant’s profile, and they want to use the text from the resume, including participation in extracurricular activities.

# Set WD
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")
options(scipen = 999)


# Libs
library(caret)
library(glmnet)
library(MLmetrics)
library(vtreat)
library(fairness)
library(ranger)

# Data
candidates <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1/G_Apr6/data/HR%20Hiring%20(Bias%20%26%20Fairness).csv')

### SAMPLE : Partitioning
set.seed(1234)
idx             <- createDataPartition(candidates$Hired,p=.7,list=F)
trainCandidates <- candidates[idx,]
testCandidates  <- candidates[-idx,]

### EXPLORE
head(as.data.frame(trainCandidates),2)

table(trainCandidates$Hired)


# Modify
drops <- c('ApplicationID', 'AgeBracket', 'Gender', 'Summary')
trainCandidates <- trainCandidates[, !(names(trainCandidates) %in% drops)]

plan <- designTreatmentsC(trainCandidates, 
                          names(trainCandidates)[2:10],
                          names(trainCandidates)[1],
                          'Yes')

treatedTrain <- prepare(plan, trainCandidates)
treatedTrain <- treatedTrain[,-grep('_catP|_catB',names(treatedTrain))]
treatedTest  <- prepare(plan, testCandidates)

fit <- ranger(as.factor(Hired)~., 
              treatedTrain, 
              importance = 'permutation',
              probability = T)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(fit)),
                       importance = importance(fit),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + theme_minimal()

# Get Predictions
trainPreds <- predict(fit, treatedTrain)
testPreds <- predict(fit, treatedTest)

# Results w/protected classes - Training
trainResults                <- data.frame(trainPreds$predictions)
trainResults$actual         <- treatedTrain$Hired
trainResults$classification <- ifelse(trainResults$Yes>=0.5,'Yes','No')
trainResults$age            <- candidates[idx,]$AgeBracket
trainResults$gender            <- candidates[idx,]$Gender

# Results w/protected classes - Testing
testResults                <- data.frame(testPreds$predictions)
testResults$actual         <- treatedTest$Hired
testResults$classification <- ifelse(testResults$Yes>=0.5,'Yes','No')
testResults$age            <- testCandidates$AgeBracket
testResults$gender         <- testCandidates$Gender

# Assess - Training
table(trainResults$classification, trainResults$actual)
Accuracy(trainResults$classification, trainResults$actual)

# Assess - Testing
table(testResults$classification, testResults$actual)
Accuracy(testResults$classification, testResults$actual)

# Model behavior
# Test for equal representation "positive class parity" for every one
# 40 and over candidate predicted to be hired, 
# how many under 40 candidates are predicted to be hired? 
# Positive class for >40 / positive class for <40
dem_parity(data = trainResults, 
           outcome = 'actual', 
           group = 'age',
           preds = 'classification', base = '40 and Over')

dem_parity(data = testResults, 
           outcome = 'actual', 
           group = 'age',
           preds = 'classification', base = '40 and Over')

dem_parity(data = trainResults, 
           outcome = 'actual', 
           group = 'gender',
           preds = 'classification', base = 'Male')


dem_parity(data = testResults, 
           outcome = 'actual', 
           group = 'gender',
           preds = 'classification', base = 'Male')

# Since gender was removed, let's figure out whats happening.  Append gender
genderDF   <- cbind(trainCandidates,gender = candidates$Gender[idx])
genderPlan <- designTreatmentsC(genderDF, names(genderDF)[2:10],'gender', 'Male')
genderDF   <-  prepare(genderPlan,genderDF)
genderDF   <- genderDF[,-grep('_catP|_catB',names(genderDF))]

# Probabliity of being Male
genderFit  <- glm(as.factor(gender)~.+0, 
                  genderDF,
                  family='binomial')

betaVals <- data.frame(coeff = names(coefficients(genderFit)), 
                       beta  = coefficients(genderFit),
                       row.names = NULL)
betaVals <- betaVals[order(betaVals$beta, decreasing = T),]
betaVals <- betaVals[complete.cases(betaVals),]
head(betaVals,10)
tail(betaVals,10)

# End
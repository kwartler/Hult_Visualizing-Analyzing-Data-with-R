#' Author: Ted Kwartler
#' Date: Mar 8 - 2023
#' Purpose: Example vtreat prep for case
#' 

# Libs
library(vtreat)

# Data io, just using one CSV but you would want to perform your joins  before this:
df <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv')

# Select the variables, we as experts think makes sense
keeps <- c("Communication", "LastContactDay", "LastContactMonth", "NoOfContacts", "DaysPassed",      
                            "PrevAttempts", "past_Outcome","Y_AcceptedOffer")


# Copied & adjusted from the c_newCar.r example script, to get 10% variable treatment, 75% training & 15% validation
trainPercentRows      <- round(nrow(df) %*% .75)
validationPercentRows <- round(nrow(df) %*% .15)

# Sample index for training
trainIdx <- sample(1:nrow(df), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(df), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- df[trainIdx,  names(df) %in% keeps]
validationSet <- df[validationIdx, names(df) %in% keeps]

# Here you combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- df[-c(trainIdx, validationIdx), names(df) %in% keeps]

### THIS IS CLASSIFICATION SO C IS USED
plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = c("Communication", "LastContactDay", "LastContactMonth",
                                            "NoOfContacts", "DaysPassed", "PrevAttempts", "past_Outcome"), 
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")

# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)

# treatedTrain is your training set for modeling
# treatedValidation is your validation set for assessment




# End
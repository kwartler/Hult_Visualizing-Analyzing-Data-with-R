#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#' Purpose: Fundraising PreProcessing

# Setwd
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")
options(scipen=999)

# Libs
library(vtreat)
library(dplyr)
library(ggplot2)
options(scipen = 999)

# Read in the data
donors<- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/F_Mar13/data/fakeDonorBureau_v2.csv')

# Examine; Here you would perform EDA
summary(donors)

# Examine names for vtreat usage
names(donors)
informativeFeatures <- names(donors)[3:19]
targetVariable      <- names(donors)[20]
successClass        <- 'Yes'

# Automated variable processing
# for **categorical** outcomes 
# i. e.will the prospective donor give Y/N
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS
plan <- designTreatmentsC(donors, 
                          informativeFeatures,
                          targetVariable, 
                          successClass)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls()[-grep('donors', ls())])

# for **numeric** outcomes 
# how much will the prospective donor give?
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR
plan <- designTreatmentsN(donors, 
                          names(donors)[3:19],
                          'Y2_DonatedAmt')

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls()[-grep('donors', ls())])

# Fictitious Data Enrichment; its a BEST PRACTICE to load all tables in one section but this script is for reference
thirdPartyData <- read.csv( 'https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/F_Mar13/data/fakeDataEnrichment.csv')

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors
leftData <- left_join(donors, thirdPartyData) 

## A taste of whats to come...for those in the know, yes we are skipping a lot of steps.
newInformativeFeatures <- names(leftData)[c(4:20,22,23)]
newInformativeFeatures
target       <- 'Y1_Donation'
successClass <- 'Yes'
plan <- designTreatmentsC(leftData,
                          newInformativeFeatures,
                          'Y1_Donation',
                          'Yes')
treatedLeftData <- prepare(plan, leftData)
fit             <- glm(as.factor(Y1_Donation) ~ ., treatedLeftData, family='binomial')

# Our first model!
summary(fit)

# Let's make it parsimonious
parismonyFit <- step(fit, direction = 'backward')

# Make some real predictions
donationProbability <- predict(parismonyFit, treatedLeftData, type='response')

head(donationProbability)

# Organize some test results
predDF <- data.frame(actual = treatedLeftData$Y1_Donation,
                     probs  = donationProbability)
ggplot(predDF, aes(x=probs, group= actual, color = actual))+geom_density()


# Assess - calculate accuracy, plot the ROC and make a confusion matrix etc.  Lots of ways to assess a model!
cutoff <- 0.5
predClass <- ifelse(donationProbability>=cutoff,'Yes','No')
table(treatedLeftData$Y1_Donation, predClass)


# End

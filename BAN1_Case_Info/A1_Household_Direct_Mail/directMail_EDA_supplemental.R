#' Author: Ted Kwartler
#' Date: Jan-25-2023
#' Purpose: Direct Mail EDA Case Supplemental
#' 

# Libs
library(dplyr)
library(ggplot2)


# Get the inhouse data as `households`
households <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv')

# Load one of the supplemental data sources
consumerDF<- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv')

##### I would do some basic EDA and plotting of individual vars then move to more complex interactions
as.data.frame(table(round(households$Age)))
ggplot(data = households) + geom_density(aes(x=Age))

##### Example 2 way EDA
plotDF <- data.frame(table(households$Gender,  households$PropertyType))

#### Let's clean up the data to remove missing gender
plotDF <- subset(plotDF, plotDF$Var1 !='')

# Stacked
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")
# Filled
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity")

#### Missing in HomePurchasePrice & quick mean imputation example
# First make it numeric in a new column
households$housePrice <- as.numeric(gsub('[$]','',households$HomePurchasePrice))

sum(is.na(households$housePrice))
households$housePrice[is.na(households$housePrice)] <- mean(households$housePrice, na.rm=TRUE)
hist(households$housePrice)

##### Feature Engineer EthnicDescription & education if you thought there was a connection
ethnicEDU <- paste(consumerDF$NetWorth, consumerDF$HomeOwnerRenter, sep = '_')
table(ethnicEDU)

##### Enrich with one of the other data sets, you may want to do this with the other csv files.  Remember to do a left join because the external data sets have more records that are not part of the inHouse customer book of business in this case.
moreData <- left_join(households, consumerDF, by ='tmpID')
head(moreData)

#### You can use complete.cases() to identify records without NA if that is the route you want to explore.  Of course you can use a function covered in class to visualize the variables with the hightest % of NA so you could drop those instead of all rows with an NA.
completeMoreData <- moreData[complete.cases(moreData),]

# End

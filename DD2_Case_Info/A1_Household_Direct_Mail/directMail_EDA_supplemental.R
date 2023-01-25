#' Author: Ted Kwartler
#' Date: Jan-25-2023
#' Purpose: Direct Mail EDA Case Supplemental
#' 

# Libs
library(dplyr)
library(ggplot2)


# Get the inhouse data as `households`
households <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')

# Load one of the supplemental data sources
consumerDF<- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/LatLon.csv')

##### I would do some basic EDA and plotting of individual vars then move to more complex interactions
table(profiles$age)
ggplot(data = profiles) + geom_histogram(aes(x=age))

##### Example 2 way EDA
plotDF <- data.frame(table(profiles$sex,  profiles$body_type))
# Stacked
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")
# Filled
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity")

#### Missing in income & quick mean imputation example
sum(is.na(profiles$income))
profiles$income[is.na(profiles$income)] <- mean(profiles$income, na.rm=TRUE)

##### Feature Engineer relationship status & education if you thought there was a connection
profiles$statEDU <- paste(profiles$status, profiles$education, sep = '_')
table(profiles$statEDU)

##### Enrich with one of the new data sets, you may want to do this with the other csv files
moreData <- left_join(profiles, latlon, by ='location')
head(moreData)

#### You can use complete.cases() to identify records without NA if that is the route you want to explore.  Of course you can use a function covered in class to visualize the variables with the hightest % of NA so you could drop those instead of all rows with an NA.
completeMoreData <- moreData[complete.cases(moreData),]

# End

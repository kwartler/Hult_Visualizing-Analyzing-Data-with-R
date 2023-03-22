#' Author: Ted Kwartler
#' Date: Mar 22, 2023
#' Purpose: Joining Supplemental
#' 

# Libraries
library(powerjoin)

# List all the files paths of interest from your local machine
# Chance the path parameter to your own folder!
allFiles <- list.files(path = '~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/BAN1_Case_Info/A1_Household_Direct_Mail',
                       pattern = '*.csv',
                       full.names = T)

# Read in a list, list apply a function to an object ie read.csv to the file paths
allDF <- lapply(allFiles, read.csv)

# Using data.table rbindlist put all the data together
households <- power_left_join(allDF, by = "tmpID")

# Since the "external" data has additional households not part of the internal BBY loyalty program, you can use complete.cases() to obtain the best records for EDA analysis
bbyLoyalty <- households[complete.cases(households),]

# You can save a copy of the joined data with this.  It will save to your local working directory.
write.csv(bbyLoyalty, 'bbyLoyalty.csv', row.names = F)

# End

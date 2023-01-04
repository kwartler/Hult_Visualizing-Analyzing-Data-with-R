
makeFakeCuData <- function(cuN      = 10000, 
                           nSKUs    = 100, 
                           minPrice = 5, 
                           maxPrice = 50, 
                           transactionN = 1000000,#conjurer::getTrans
                           outliers = T,#conjurer::getTrans
                           monthSpike = 12, #conjurer::getTrans
                           peakN = 'y', #'y'=1x/yr, 'q'=4x/yr, 'm'= 12x/yr
                           startDate = "2022-01-01", #transaction window
                           endDate = "2022-12-31", #transaction window
                           paretoHighCu = 80, #customer allocation ie 80% of transactions allocated to 20% of customers 
                           paretoLowCu = 20, #customer allocation ie 80% of transactions allocated to 20% of customers
                           paretoHighSKU = 70,#SKU allocation ie 70% of transactions allocated to 30% of the products offered 
                           paretoLowSKU = 30,#SKU allocation ie 70% of transactions allocated to 30% of the products offered 
                           augment = T, # add more customer info from synthetic table
                           productHierarchy = data.frame(category = rep(c('Food','Non-Food')),
                                                         subcategory = c('Beverages','Sanitary','Dairy','Household'))){
  require(conjurer)
  require(generator)
  require(lubridate)
  require(dplyr)
  
  # Build the customers
  customers <- buildCust(numOfCust =  cuN)
  
  # There is a prob with the function in conjurer w/replace = T
  # Chunking is a quick way to fix
  chk <- cuN / 50
  ageism <- list()
  for(i in 1:chk){
    custAge <- as.data.frame(round(buildNum(n = 50, 
                                            st = 18, 
                                            en = 81, 
                                            disp = 0.5, 
                                            outliers = 1)),
                             row.names = NULL)
    ageism[[i]] <- custAge
  }
  ageism <- unlist(ageism)
  names(ageism) <- rep('CuAge', length(ageism))
  
  # Age and phone numbers
  customer2age       <- data.frame(customer = customers, age =  ageism)
  customer2age$phone <-  r_phone_numbers(nrow(customer2age), use_hyphens = T, use_parentheses = T)
  
  # Build out the product catalog and transactions
  products     <- buildProd(numOfProd = nSKUs, minPrice = minPrice, maxPrice = maxPrice)
  transactions <- genTrans(cycles = "y", spike = monthSpike, outliers = outliers, transactions = transactionN)
  
  # Add proper hms not just dayNum from conjurer
  rDate <- function(sDate, eDate, nDate){   
    lenDate <- nDate
    seqDays <- seq.POSIXt(as.POSIXct(sDate), as.POSIXct(eDate), by="secs")  
    aDay <- runif(lenDate, 1, length(seqDays))  
    Date <- seqDays[aDay]  
  }
  
  # Change to the timestamp
  synthTime <- rDate(startDate, endDate, nrow(transactions))
  synthTime <- sort(synthTime)
  
  # Match original dayNum/mthNum append the hms
  transactions$timestamp <- synthTime
  
  # Customer Transaction Allocation
  customer2transaction <- buildPareto(customers, transactions$transactionID, pareto = c(paretoHighCu,paretoLowCu))
  names(customer2transaction) <- c('transactionID', 'customer')
  
  # Product Type Allocation
  products <- data.frame(products, productHierarchy)
#  productHierarchy <- as.data.frame(cbind(category,subcategory,1:nrow(products)))
#  products <- cbind(products, productHierarchy[,c("category","subcategory")])
  product2transaction <- buildPareto(products$SKU,transactions$transactionID,pareto = c(paretoHighSKU,paretoLowSKU))
  names(product2transaction) <- c('transactionID', 'SKU')
  
  # Create Augmented Table
  aug <- read.csv('augmentDataTable.csv')
  if(cuN>nrow(aug)){stop(paste('there are only 15k rows in the augmented table but you have cuN:', cuN))}
  idx <- sample(1:nrow(aug), cuN)
  aug <- data.frame(customer = customers, aug[idx,])
  
  finalDF <- left_join(product2transaction, customer2transaction, by = "transactionID")
  finalDF <- left_join(finalDF, transactions, by = "transactionID")
  finalDF <- left_join(finalDF, customer2age,  by = "customer")
  finalDF <- left_join(finalDF, products, by = "SKU")
  finalDF <- left_join(finalDF, aug, by = "customer")
  
  # Final Ordering
  finalDF$timestamp <- parse_date_time(finalDF$timestamp,"y m d HMS")
  finalDF <- finalDF[order(finalDF$timestamp),]
  return(finalDF)
}

# End
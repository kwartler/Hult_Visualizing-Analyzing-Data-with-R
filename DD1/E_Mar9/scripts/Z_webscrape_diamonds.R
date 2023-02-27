allURLs <- paste0('https://diamondsdirect.com/diamonds/?from=',1:722) # Just ensure last page

# To avoid black listing breaking up into multiple loops 
allURLS <- list()
for(i in 1:length(allURLs)){
  print(i)
  waitTime <- sample(seq(2,3, by =0.05),1)
  Sys.sleep(waitTime)
  pg <- read_html(allURLs[i])
  individualRockURL <- pg %>%
    html_nodes('.card-inner') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  res <- paste0('https://diamondsdirect.com',individualRockURL)
  allURLS[[i]] <- res
}
x <- unlist(allURLS)
x <- x[!duplicated(x)]

# Now read specific diamond info
allDiamonds <- list()
for(i in 1:length(x)){
  waitTime <- sample(seq(2,3, by =0.05),1)
  Sys.sleep(waitTime)
  print(i)
  pg <- read_html(x[i])
  price <- pg %>%
    html_node('.price--withoutTax') %>% html_text
  imgURL <- pg %>%
    html_nodes('.product-image-section') %>%
    html_node('img') %>%
    html_attr('src')
  stats <- pg %>% html_nodes('.product-info-list') %>% html_nodes('span')%>% html_text()
  statLabels <- pg %>% html_nodes('.product-info-list') %>% html_nodes('strong')%>% html_text()
  statLabels <- gsub(':','',statLabels)
  stats <- as.data.frame(t(stats))
  names(stats) <- statLabels
  stats$SKU <- paste0('sku_',stats$SKU)
  stats$Carat <- as.numeric(stats$Carat)
  tmp <- as.numeric(unlist(strsplit(stats$Measurements,'x|-')))
  stats$x <- tmp[1]
  stats$y <- tmp[2]
  stats$z <- tmp[3]
  stats$Cut <- ifelse(is.null(stats$Cut), 'NA',stats$Cut)
  stats$price <- as.numeric(gsub('[$]|,','',price))
  stats$listingURL <-x[i] 
  stats$imgURL <- imgURL
  allDiamonds[[i]] <- stats 
}

xChk <- lapply(allDiamonds, dim)
xChk <- do.call(rbind, allDiamonds) 
 
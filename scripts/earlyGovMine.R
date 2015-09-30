rm(list=ls())

library(RCurl);library(XML);library(stortingAlpha);library(RColorBrewer)

url <- "https://www.regjeringen.no/no/om-regjeringa/tidligere/ministerier_regjeringer/opplosningen-av-unionen-med-sverige/departementsinndeling-1905-1940/regjeringsliste-1905-1945/id438665/"


htmlTree <- getURL(url, ssl.verifypeer=FALSE)
htmlTree <- htmlTreeParse(htmlTree, useInternalNodes = TRUE)
  
htmlTreeURL <- gsub("^\\r\\n[[:space:]]+|[[:space:]]+$", "", unlist(xpathSApply(htmlTree, "//a/@href")))



linker <- unique(as.character(htmlTreeURL[which(grepl("/no/om-regjeringa/tidligere/ministerier_regjeringer", htmlTreeURL))])[4:24])

test <- paste0("https://regjeringen.no", linker[1])

#test <- lapply(test, function(x) getURL(x, .opts=curlOptions(followlocation=TRUE)))
test2 <- getURL(test, .opts = curlOptions(followlocation=TRUE))

#hmm <- lapply(test, function(x) htmlTreeParse(x, useInternalNodes = TRUE))
test3 <- htmlTreeParse(test2, useInternalNodes = TRUE)

htmlTree4 <- xpathSApply(test3, "//p[@class='A LEFT']", xmlValue)

htmlTreeTables <- gsub("\\n", " ", xpathSApply(test3, "//table", xmlValue))


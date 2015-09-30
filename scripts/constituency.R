rm(list=ls())

library(RCurl);library(XML);library(gsubfn)

url <- "https://en.wikipedia.org/wiki/List_of_municipalities_of_Norway"



htmlTree <- getURL(url, ssl.verifypeer=FALSE)


tables <- readHTMLTable(htmlTree)[2]
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

data <- data.frame(tables)

names(data)

munList <- strsplit(as.character(data$NULL.Name), " \\!")

for(i in 1:length(munList)){
  munList[[i]][1] <- ifelse(is.na(munList[[i]][2]), munList[[i]][1], munList[[i]][2])
}

munList <- paste(sapply(munList, "[[", 1), collapse = "|")
munList <- gsub("\\|Re\\|", "|", munList)

data("writtenAll")

test<-grepl(munList, writtenAll$question)

summary(test)

head(test, 20)

writtenAll$munMent <- test



rm(list=ls())

library(dplyr)

#ches <- read.csv("C:\\Users\\Martin\\Dropbox\\Pre-PhD\\R\\CHES_2014.csv")
#ches2 <- read.csv("C:\\Users\\Martin\\Dropbox\\Pre-PhD\\R\\chestrend.csv", sep = "\t")

cmp <- read.csv("C:\\Users\\Martin\\Dropbox\\Pre-PhD\\R\\cmp2015.csv")

cmp <- cmp[which(cmp$countryname=="Norway" & cmp$date>199001),]

cmp$edate <- as.Date(as.character(cmp$edate), "%d/%m/%Y")
cmp$election_date <- as.numeric(gsub("-", "", as.character(cmp$edate)))
cmp$partynameshort <- factor(sapply(strsplit(as.character(cmp$partyname), " "), "[[", 1), labels = c("DNA", "FrP", "H", "KrF", "Sp", "SV", "V"))

factor(cmp$partynameshort, labels = c("DNA", "FrP", "H", "KrF", "Sp", "SV", "V"))

data("writtenAll")
factor(test$fromParty)

test <- writtenAll %>%
  group_by(fromParty, election_date) %>%
  summarise(left_right = left_right[1])
test$election_date <- as.numeric(gsub("-", "", as.character(test$election_date)))

test2 <- merge(x = test, y = cmp[,c("election_date", "partynameshort", "rile")], 
               by.x = c("election_date", "fromParty"), 
               by.y = c("election_date", "partynameshort"), all.x = T)

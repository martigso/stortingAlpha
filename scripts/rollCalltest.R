

rm(list = ls())

library(stortingAlpha);library(stringr);library(dplyr)


data("AllVotes")


st1900 <- AllVotes[which(AllVotes$chamber=="Stortinget" & AllVotes$year==1900 & AllVotes$rollcall==1), ]
st1900$text <- as.character(st1900$text)
st1900$text <- str_trim(st1900$text)





load("C:\\Users\\Martin\\Dropbox\\Master\\Vitass\\Stortingsprosjektet\\Stortinget\\infodata\\norPoliticianArchive1814_1940.rda")

rep1900 <- norPoliticianArchive1814_1940 %>%
  filter(Period==1900)

rep1900[,4:5] <- apply(rep1900[,4:5], 2, as.character)
rep1900 <- rep1900[which(duplicated(rep1900$PersonNumber)==FALSE),]

rollcallDf <- data.frame(matrix(nrow=141, ncol=53))

colnames(rollcallDf) <- c("FirstName", "LastName", sapply(1:51, function(x) paste0("rv0", x)))

rollcallDf$FirstName <- rep1900$FirstName

rollcallDf$LastName <- rep1900$LastName


splits <- strsplit(st1900$text, "[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}herrer[[:space:]]{0,1}var:|[[:alpha:]]rav[[:alpha:]]rende|lfiavaeitaide")

splits <- lapply(splits, function(x) strsplit(x[2:3], ","))
yea <- sapply(splits, "[[", 1)
nay <- sapply(splits, "[[", 2)

yea <- lapply(yea, function(x) str_trim(x))
names(yea) <- colnames(rollcallDf[, 3:ncol(rollcallDf)])
yea <- lapply(yea, function(x) ifelse(rollcallDf$LastName %in% x==TRUE, "Yea", NA))

for(i in colnames(rollcallDf)[3:ncol(rollcallDf)]){
  rollcallDf[,i] <- yea[[i]]
}


nay <- lapply(nay, function(x) str_trim(x))
names(nay) <- colnames(rollcallDf[, 3:ncol(rollcallDf)])
nay <- lapply(nay, function(x) ifelse(rollcallDf$LastName %in% x==TRUE, "Nay", NA))

for(i in colnames(rollcallDf)[3:ncol(rollcallDf)]){
  rollcallDf[,i] <- ifelse(is.na(rollcallDf[,i])==TRUE, nay[[i]], rollcallDf[,i])
  }


rm(list=ls())

library(ministersNor);library(devtools);library(stortingAlpha);library(uacd);library(dplyr);library(gsubfn)

load("C:\\Users\\Martin\\Desktop\\writtenAll.RData")

writtenAll <- writtenAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "answBy", "toDepMinister", "question", "fromParty", "session")]

writtenAll[c("type", "from", "answBy", "toDepMinister", "question", "fromParty")] <- apply(writtenAll[c("type", "from", "answBy", "toDepMinister", "question", "fromParty")], 2, as.character)

temp <- writtenAll[which(grepl("Besvart:", writtenAll$answBy)==TRUE),]
tempAnswBy <- na.omit(temp$answBy[sapply(temp$answBy, function(x) grep("Bortfaller|Besvart:|Sp[[:alpha:]]rsm[[:alpha:]]let er trukket", x)==1)==TRUE])
tempAnswBy <- unlist(strsplit(tempAnswBy, "^.*?SHD |^.*?president |^.*?ministerens kontor |^.*?ministeren "))

tempAnswBy <- tempAnswBy[which(nchar(tempAnswBy)>0)]
temp$tempAnswBy <- tempAnswBy

writtenAll$answBy <- ifelse(grepl("Besvart:", writtenAll$answBy)==TRUE, tempAnswBy, writtenAll$answBy)

rm(temp, tempAnswBy)

writtenAll$dateAskNum <- as.numeric(gsub("-", "", as.character(writtenAll$dateAsked)))


################

data("ministers")
ministers$fullname <- paste(ministers$first_name, ministers$last_name)

ministersCab <- ministers %>%
  filter(start_year>1990) %>%
  group_by(cabinet_name) %>%
  summarise(dateStartNum = as.numeric(gsub("-", "", as.character(From[1]))),
            dateEndNum = as.numeric(gsub("-", "", as.character(To[1]))))

solberg <- data.frame(cabinet_name = "Solberg I", dateStartNum = 20131016, dateEndNum = NA)
ministersCab <- data.frame(rbind(ministersCab, solberg))

writtenAll$cabinet_name <- NA
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik I")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik I")],
                                 "Bondevik I", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik II")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik II")],
                                 "Bondevik II", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland III")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland III")],
                                 "Brundtland III", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland IV")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland IV")],
                                 "Brundtland IV", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Jagland I")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Jagland I")],
                                 "Jagland I", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg I")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg I")],
                                 "Stoltenberg I", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg II")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg II")],
                                 "Stoltenberg II", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg III")] &
                                   writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg III")],
                                 "Stoltenberg III", writtenAll$cabinet_name)
writtenAll$cabinet_name <- ifelse(writtenAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Solberg I")],
                                 "Solberg I", writtenAll$cabinet_name)

rm(solberg, ministersCab)

writtenAll$cabinet_name[which(is.na(writtenAll$cabinet_name))] <- "Stoltenberg III"

ministersTemp<-ministers[which(duplicated(ministers$nsd_id)==FALSE), c("nsd_id", "fullname", "party", "cabinet_name")]

writtenAll$answBy[which(grepl("Enger", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Enger", ministers$first_name)==TRUE)]
writtenAll$answBy[which(grepl("Åslaug", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Åslaug", ministers$first_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Aure", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Aure", ministers$last_name)==TRUE)][1]
#writtenAll$answBy[which(grepl("Berget", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Berget", ministers$last_name)==TRUE)][1]
#writtenAll$answBy[which(grepl("Øyangen", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Øyangen", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Ingebrigtsen", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Ingebrigtsen", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Bjurstrøm", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjurstrøm", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Bjørnøy", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjørnøy", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Jan Henry", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Jan Henry", ministers$first_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Kosmo", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Kosmo", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Krohn", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Krohn", ministers$first_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Djupedal", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Djupedal", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Rønbeck", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Rønbeck", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Brustad", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Brustad", ministers$last_name)==TRUE)][1]
writtenAll$answBy[which(grepl("Norman", writtenAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Norman", ministers$last_name)==TRUE)][1]

writtenAll$dateAskNum <- NULL

writtenAll <- merge(x=writtenAll, y=ministersTemp[, c("fullname", "party", "nsd_id")], by.x=c("answBy"), by.y=c("fullname"), all.x = TRUE)
writtenAll <- merge(x=writtenAll, y=ministersTemp[, c("fullname", "cabinet_name")], by.x=c("answBy", "cabinet_name"), by.y=c("fullname", "cabinet_name"), all.x = TRUE)

names(writtenAll)[12] <- "answByParty"

rm(ministersTemp)

writtenAll <- writtenAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy",
                          "toDepMinister", "answByParty", "nsd_id", "session", "cabinet_name", "question")]

#writtenAll$fromParty <- as.character(factor(writtenAll$fromParty, labels = c("DNA", "FrP", "H", "Kp", "KrF", "MDG",
#                                                                         "RV", "Sp", "SV", "TF", "Uav", "Uavh", "V")))

writtenAll$year <- substring(as.character(writtenAll$dateAsked), 1, 4)

writtenAll$fromParty <- ifelse(writtenAll$fromParty=="TF" & writtenAll$year>1998, "Kp", writtenAll$fromParty)

###################


###################
data("ParlGov")

nor<-ParlGov %>%
  filter(country_name=="Norway" & seats>0 & year>1990) %>%
  group_by(party_id, cabinet_name) %>%
  summarise(party_name_short=as.character(party_name_short)[1],
            cabinet_party = cabinet_party[1],
            election_date = election_date[1],
            seats = seats[1],
            left_right = left_right[1]) %>%
  ungroup() %>%
  arrange(election_date)

nor$party_name_short <- ifelse(nor$party_name_short=="Kp" & nor$cabinet_name=="Bondevik I", "TF", nor$party_name_short)
nor$party_name_short <- ifelse(nor$party_name_short=="Fr", "FrP", nor$party_name_short)
nor$cabinet_name <- as.character(nor$cabinet_name)
nor$cabinet_name <- ifelse(nor$cabinet_name=="Jagland", "Jagland I", nor$cabinet_name)

#Add latest election to ParlGov data
election2013 <- data.frame(party_id = factor(c(104, 1435, 351, 1538, 702, 647, 81, 2222)),
                           party_name_short = c("DNA", "H", "FrP", "KrF", "Sp", "V", "SV", "MDG"),
                           cabinet_party = factor(c(0, 1, 1, 0, 0, 0, 0, 0)),
                           election_date = factor("2013-09-09"),
                           seats = c(55, 48, 29, 10, 10, 9, 7, 1),
                           left_right = rep(NA, 8),
                           cabinet_name = "Solberg I")

nor <- data.frame(rbind(nor, election2013))

writtenAll <- merge(x=writtenAll, y=nor, by.x = c("fromParty", "cabinet_name"), by.y = c("party_name_short", "cabinet_name"), all.x = TRUE)
names(writtenAll)

rm(ministers, nor, ParlGov, election2013)

writtenAll <- writtenAll[, c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy", "answByParty", "toDepMinister",
                             "nsd_id", "session", "year", "question", "party_id", "cabinet_name", "cabinet_party", "election_date",
                             "seats", "left_right")]
#save(writtenAll, file = "C:\\Users\\Martin\\Desktop\\writtenAll.RData")

#use_data(writtenAll, overwrite=TRUE)


rm(list=ls())

library(ministersNor);library(devtools);library(stortingAlpha);library(uacd);library(dplyr);library(gsubfn)

load("C:\\Users\\Martin\\Desktop\\questAll.RData")

questAll <- questAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "answBy", "toDepMinister", "question", "fromParty", "session")]

questAll[,c("type", "from", "answBy", "toDepMinister", "question", "fromParty")] <- apply(questAll[,c("type", "from", "answBy", "toDepMinister", "question", "fromParty")], 2, as.character)

questAll$fromParty <- ifelse(grepl("Skei", questAll$fromParty)==TRUE, "V", questAll$fromParty)


temp <- questAll[which(grepl("Besvart:", questAll$answBy)==TRUE),]
tempAnswBy <- na.omit(temp$answBy[sapply(temp$answBy, function(x) grep("Bortfaller|Besvart:|Sp[[:alpha:]]rsm[[:alpha:]]let er trukket", x)==1)==TRUE])
tempAnswBy <- unlist(strsplit(tempAnswBy, "^.*?BRU |^.*?president |^.*?ministerens kontor |^.*?ministeren "))

tempAnswBy <- tempAnswBy[which(nchar(tempAnswBy)>0)]
temp$tempAnswBy <- tempAnswBy

questAll$answBy <- ifelse(grepl("Besvart:", questAll$answBy)==TRUE, tempAnswBy, questAll$answBy)

rm(temp, tempAnswBy)

questAll$dateAskNum <- as.numeric(gsub("-", "", as.character(questAll$dateAsked)))


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

questAll$cabinet_name <- NA
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik I")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik I")],
                                  "Bondevik I", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik II")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik II")],
                                  "Bondevik II", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland III")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland III")],
                                  "Brundtland III", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland IV")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland IV")],
                                  "Brundtland IV", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Jagland I")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Jagland I")],
                                  "Jagland I", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg I")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg I")],
                                  "Stoltenberg I", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg II")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg II")],
                                  "Stoltenberg II", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg III")] &
                                    questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg III")],
                                  "Stoltenberg III", questAll$cabinet_name)
questAll$cabinet_name <- ifelse(questAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Solberg I")],
                                  "Solberg I", questAll$cabinet_name)

rm(solberg, ministersCab)

ministersTemp<-ministers[which(duplicated(ministers$nsd_id)==FALSE), c("nsd_id", "fullname", "party", "cabinet_name")]

questAll$answBy[which(grepl("Enger", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Enger", ministers$first_name)==TRUE)]
questAll$answBy[which(grepl("Åslaug", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Åslaug", ministers$first_name)==TRUE)][1]
questAll$answBy[which(grepl("Aure", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Aure", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Berget", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Berget", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Øyangen", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Øyangen", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Ingebrigtsen", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Ingebrigtsen", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Bjurstrøm", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjurstrøm", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Bjørnøy", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjørnøy", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Jan Henry", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Jan Henry", ministers$first_name)==TRUE)][1]
questAll$answBy[which(grepl("Kosmo", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Kosmo", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Krohn", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Krohn", ministers$first_name)==TRUE)][1]
questAll$answBy[which(grepl("Djupedal", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Djupedal", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Rønbeck", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Rønbeck", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Brustad", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Brustad", ministers$last_name)==TRUE)][1]
questAll$answBy[which(grepl("Norman", questAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Norman", ministers$last_name)==TRUE)][1]

questAll$dateAskNum <- NULL

questAll <- merge(x=questAll, y=ministersTemp[, c("fullname", "party", "nsd_id")], by.x=c("answBy"), by.y=c("fullname"), all.x = TRUE)
questAll <- merge(x=questAll, y=ministersTemp[, c("fullname", "cabinet_name")], by.x=c("answBy", "cabinet_name"), by.y=c("fullname", "cabinet_name"), all.x = TRUE)

names(questAll)[12] <- "answByParty"

rm(ministersTemp)

questAll <- questAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy",
                        "toDepMinister", "answByParty", "nsd_id", "session", "cabinet_name", "question")]

###### Must run on reload from here
#questAll$fromParty <- as.character(factor(questAll$fromParty, labels = c("DNA", "FrP", "H", "Kp", "KrF", "MDG",
#                                                            "RV", "Sp", "SV", "TF", "Uav", "Uavh", "V")))

questAll$fromParty <- ifelse(questAll$from=="Jan Simonsen", "FrP", questAll$fromParty)
questAll$fromParty <- ifelse(questAll$from=="Arne Haukvik", "Sp", questAll$fromParty)
questAll$fromParty <- ifelse(questAll$from=="Ellen Chr. Christiansen", "FrP", questAll$fromParty)
questAll$fromParty <- ifelse(questAll$from=="Jørn L. Stang", "FrP", questAll$fromParty)

questAll$year <- substring(as.character(questAll$dateAsked), 1, 4)

questAll$fromParty <- ifelse(questAll$fromParty=="TF" & questAll$year>1998, "Kp", questAll$fromParty)

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

questAll <- merge(x=questAll, y=nor, by.x = c("fromParty", "cabinet_name"), by.y = c("party_name_short", "cabinet_name"), all.x = TRUE)
names(questAll)

rm(ministers, nor, ParlGov, election2013)

questAll <- questAll[, c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy", "answByParty", "toDepMinister",
                         "nsd_id", "session", "year", "question", "party_id", "cabinet_name", "cabinet_party", "election_date", "seats",
                         "left_right")]

#save(questAll, file = "C:\\Users\\Martin\\Desktop\\questAll.RData")

#use_data(questAll, overwrite=TRUE)

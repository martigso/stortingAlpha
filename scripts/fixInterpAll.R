rm(list=ls())

library(ministersNor);library(devtools);library(stortingAlpha);library(uacd);library(dplyr);library(gsubfn)


load("C:\\Users\\Martin\\Desktop\\interpAll.RData")

interpAll <- interpAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "answBy", "toDepMinister", "question", "fromParty", "session")]

interpAll[,c("type", "from", "answBy", "toDepMinister", "question", "fromParty")] <- apply(interpAll[,c("type", "from", "answBy", "toDepMinister", "question", "fromParty")], 2, as.character)

interpAll$dateAskNum <- as.numeric(gsub("-", "", as.character(interpAll$dateAsked)))


data("ministers")
ministers$fullname <- paste(ministers$first_name, ministers$last_name)

ministersCab <- ministers %>%
  filter(start_year>1990) %>%
  group_by(cabinet_name) %>%
  summarise(dateStartNum = as.numeric(gsub("-", "", as.character(From[1]))),
            dateEndNum = as.numeric(gsub("-", "", as.character(To[1]))))

solberg <- data.frame(cabinet_name = "Solberg I", dateStartNum = 20131016, dateEndNum = NA)
ministersCab <- data.frame(rbind(ministersCab, solberg))

interpAll$cabinet_name <- NA
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik I")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik I")],
                                 "Bondevik I", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Bondevik II")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Bondevik II")],
                                 "Bondevik II", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland III")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland III")],
                                 "Brundtland III", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Brundtland IV")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Brundtland IV")],
                                 "Brundtland IV", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Jagland I")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Jagland I")],
                                 "Jagland I", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg I")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg I")],
                                 "Stoltenberg I", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg II")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg II")],
                                 "Stoltenberg II", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum <= ministersCab$dateEndNum[which(ministersCab$cabinet_name == "Stoltenberg III")] &
                                   interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Stoltenberg III")],
                                 "Stoltenberg III", interpAll$cabinet_name)
interpAll$cabinet_name <- ifelse(interpAll$dateAskNum >= ministersCab$dateStartNum[which(ministersCab$cabinet_name == "Solberg I")],
                                 "Solberg I", interpAll$cabinet_name)

rm(solberg, ministersCab)

ministersTemp<-ministers[which(duplicated(ministers$nsd_id)==FALSE), c("nsd_id", "fullname", "party", "cabinet_name")]

interpAll$answBy[which(grepl("Enger", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Enger", ministers$first_name)==TRUE)]
interpAll$answBy[which(grepl("Åslaug", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Åslaug", ministers$first_name)==TRUE)][1]
interpAll$answBy[which(grepl("Aure", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Aure", ministers$last_name)==TRUE)][1]
#interpAll$answBy[which(grepl("Berget", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Berget", ministers$last_name)==TRUE)][1]
#interpAll$answBy[which(grepl("Øyangen", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Øyangen", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Ingebrigtsen", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Ingebrigtsen", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Bjurstrøm", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjurstrøm", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Bjørnøy", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Bjørnøy", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Jan Henry", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Jan Henry", ministers$first_name)==TRUE)][1]
interpAll$answBy[which(grepl("Kosmo", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Kosmo", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Krohn", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Krohn", ministers$first_name)==TRUE)][1]
interpAll$answBy[which(grepl("Djupedal", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Djupedal", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Rønbeck", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Rønbeck", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Brustad", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Brustad", ministers$last_name)==TRUE)][1]
interpAll$answBy[which(grepl("Norman", interpAll$answBy)==TRUE)] <- ministers$fullname[which(grepl("Norman", ministers$last_name)==TRUE)][1]

interpAll$dateAskNum <- NULL

interpAll <- merge(x=interpAll, y=ministersTemp[, c("fullname", "party", "nsd_id")], by.x=c("answBy"), by.y=c("fullname"), all.x = TRUE)
interpAll <- merge(x=interpAll, y=ministersTemp[, c("fullname", "cabinet_name")], by.x=c("answBy", "cabinet_name"), by.y=c("fullname", "cabinet_name"), all.x = TRUE)

names(interpAll)[12] <- "answByParty"
rm(ministersTemp)

interpAll <- interpAll[,c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy",
                          "toDepMinister", "answByParty", "nsd_id", "session", "cabinet_name", "question")]

#interpAll$fromParty <- as.character(factor(interpAll$fromParty, labels = c("DNA", "FrP", "H", "Kp", "KrF", "MDG",
#                                                                           "RV", "Sp", "SV", "V")))

interpAll$year <- substring(as.character(interpAll$dateAsked), 1, 4)


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

interpAll <- merge(x=interpAll, y=nor, by.x = c("fromParty", "cabinet_name"), by.y = c("party_name_short", "cabinet_name"), all.x = TRUE)
names(interpAll)

rm(ministers, nor, ParlGov, election2013)

interpAll <- interpAll[, c("docId", "dateAsked", "dateAnsw", "type", "from", "fromParty", "answBy", "answByParty", "toDepMinister",
                           "nsd_id", "session", "year", "question", "party_id", "cabinet_name", "cabinet_party", "election_date", "seats",
                           "left_right")]

#####Don't run#####
#save(interpAll, file = "C:\\Users\\Martin\\Desktop\\interpAll.RData")
###################

#use_data(interpAll, overwrite=TRUE)

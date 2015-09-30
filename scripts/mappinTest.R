rm(list=ls())


data("interpAll")
data("questAll")
data("writtenAll")
test<-rbind(interpAll, questAll, writtenAll)

summary(factor(test$cabinet_party))

names(test)
(1586/(30615+1586))*100
library(ggplot2);library(dplyr)
theme_set(theme_bw())

test2<-test %>%
  filter(type!="Spørsmål ved møtets slutt" | type!="Spørsmål til presidentskapet" | type!="Fra representanten") %>%
  group_by(session, type) %>%
  summarise(nQest = length(type))

ggplot(test2, aes(x = session, y=nQest, group=type, color = type, fill = type)) +
  geom_path(stat="identity", size=1.4)+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust = 1))

test$dur <- as.numeric(test$dateAnsw-test$dateAsked)


test <- test[which(test$dur>=0 & test$type=="Skriftlig spørsmål"),]
test$dur <- ifelse(test$dur<0, NA, test$dur)
test$event <- 1

library(survival)
summary(test$dur)

str(test)
reg <- survreg(Surv(dur, event)~seats+left_right, data=test[which(test$dur>0),], dist = "weibull", subset = test$type[which(test$dur>0)]=="Skriftlig spørsmål")

summary(reg)

summary(factor(ifelse(test$fromParty==test$answByParty, "Same", "Diff")))
summary(factor(test$cabinet_party))
levels(factor(test$type))

(946/nrow(test))*100

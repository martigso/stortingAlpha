  
rm(list=ls())
library(dplyr);library(ministersNor);library(XML);library(RCurl);library(gsubfn);library(stortingAlpha)

data(questAll)
data(interpAll)
data(writtenAll)
questAll <- data.frame(rbind(questAll, interpAll, writtenAll))

questAll$dur<-as.numeric(questAll$dateAnsw-questAll$dateAsked)
questAll$event<-ifelse(is.na(questAll$dateAnsw)==TRUE, 0, 1)
questAll$oneWeek<-ifelse(questAll$dur<7, 0, 1)
questAll$type<-factor(questAll$type, levels = c("Spørretimespørsmål", "Interpellasjon", "Skriftlig spørsmål","Muntlig spørsmål", 
                                                "Fra representanten", "Spørsmål til presidentskapet", "Spørsmål ved møtets slutt"))
questAll$coalition_partner<-ifelse(questAll$fromParty!=questAll$answByParty & questAll$cabinet_party==1, 1, 0)
questAll$questionLength<-nchar(questAll$question)
#questAll$lrCentAbs<-abs(questAll$left_right-mean(questAll$left_right, na.rm = TRUE))

temp <- questAll %>%
  filter(cabinet_party==1) %>% 
  group_by(party_id, year) %>%
  summarise(lrCab=left_right[1]) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(lrCab=mean(lrCab, na.rm=TRUE))


questAll <- merge(x=questAll, y=temp, by="year", all.x = TRUE)  
rm(temp)

questAll$ideDist <- abs(questAll$lrCab-questAll$left_right)
summary(questAll$ideDist)

summary(questAll$coalition_partner)
library(survival)

reg <- survreg(Surv(dur, event)~factor(cabinet_party)*ideDist+seats*ideDist+questionLength, data=questAll, dist = "weibull", 
               subset = questAll$type=="Interpellasjon" & questAll$dur>0)
summary(reg)

pred1 <- with(questAll, data.frame(ideDist = median(ideDist, na.rm = TRUE),
                                   seats = min(seats, na.rm = TRUE):max(seats, na.rm = TRUE),
                                   questionLength = median(questionLength, na.rm = TRUE),
                                   cabinet_party = 0))

pred1 <- predict(reg, newdata = pred1, se=T, type="response")
pred1$mod <- "not coal partner"

pred2 <- with(questAll, data.frame(ideDist = median(ideDist, na.rm = TRUE),
                                   seats = min(seats, na.rm = TRUE):max(seats, na.rm = TRUE),
                                   questionLength = median(questionLength, na.rm = TRUE),
                                   cabinet_party = 0))

pred2 <- predict(reg, newdata = pred2, se=T, type="response")
pred2$mod <- "coal partner"

pred <- data.frame(fit=c(pred1$fit, pred2$fit), se=c(pred1$se.fit, pred2$se.fit), mod=c(rep(pred1$mod, 67), rep(pred2$mod, 67)))

pred$ideDist <- min(questAll$seats, na.rm = TRUE):max(questAll$seats, na.rm = TRUE)
pred$upper <- pred$fit+1.96*pred$se
pred$lower <- pred$fit-1.96*pred$se

library(ggplot2);library(grid)
theme_set(theme_bw())

ggplot(pred, aes(x=ideDist, y=fit, color=mod, fill=mod))+
  geom_line() +
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2)+
  scale_x_continuous(breaks=seq(0,6,1), expand = c(0,0))+
  scale_y_continuous(breaks=seq(0,15,1), expand = c(0,0))+
  scale_color_manual(values=c("#045454", "#7A7A00"))+
  scale_fill_manual(values=c("#045454", "#7A7A00"))+
  labs(x="Ideological distance from cabinet", y="Predicted probability", color=NULL, fill=NULL)+
  theme(legend.position=c(.15, .15),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

summary(reg$y)
summary(predict(reg, type="response"))
rm(pred, pred1, pred2, reg)


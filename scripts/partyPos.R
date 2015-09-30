
rm(list=ls());dev.off()

library(austin);library(tm);library(gsubfn);library(ggplot2)
files <- list.files("D:\\partiprogrammer\\Oversatt", pattern = "[[:digit:]].txt")

textList <- lapply(files, function(x) readLines(paste0("D:\\partiprogrammer\\Oversatt\\", x)))
names(textList) <- paste0(gsub(".txt", "", files), "_red")

textList <- textList[1:61]

textList <- lapply(textList, tolower)

textList <- lapply(textList, function(x) gsub("liberal left", "liberalleft", x))

textList <- lapply(textList, function(x) gsub("liberals", "liberalparty", x))

textList <- lapply(textList, function(x) gsub("conservartives", "conservativeparty", x))

textList <- lapply(textList, function(x) gsub("labor party", "laborparty", x))

sapply(names(textList), function(x) writeLines(textList[[x]], con=paste0("D:\\partiprogrammer\\Oversatt\\", x, ".txt")))


docs <- Corpus(DirSource("D:\\partiprogrammer\\Oversatt", pattern = "_red.txt"))

docs <- tm_map(docs, content_transformer(removeNumbers))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, "eng")
docs <- tm_map(docs, removeWords, stopwords("eng"))


dtm <- TermDocumentMatrix(docs)

termMat <- as.matrix(dtm)

morethanthree <- apply(termMat, 1, function(x) sum(x)>5)
termMat <- termMat[morethanthree,]

termMat <- wfm(termMat)

colnames(termMat)

results <- wordfish(termMat, dir = c(8, 31))

scores <- data.frame(summary(results)$scores)
scores$party <- sapply(strsplit(rownames(scores), "[[:digit:]]"), "[[", 1)
scores$year <- unlist(strapply(rownames(scores), "[0-9]{4,4}"))


theme_set(theme_bw())
pdf("C:\\Users\\Martin\\Dropbox\\Master\\Vitass\\Stortingsprosjektet\\Stortinget\\kode_tekstles\\Plots\\partyPos.pdf")
ggplot(scores, aes(x=year, y=Estimate, group=party, color=party))+
  geom_pointrange(aes(ymax=Upper, ymin=Lower), position = position_dodge(width=.5), size=1)+
  scale_color_manual(values = c("#750000", "#0C4F00", "red", "lightblue", "#15078C", "#F2FF00", "darkcyan", "green"))+
  coord_flip()+
  labs(x="Year", y="Estimate", color=NULL)+
  theme(legend.position=c(.8,.8),
        legend.key=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())
dev.off()

weight<-data.frame(words=results$words, psi=results$psi, beta=results$beta)

ggplot(weight, aes(x=beta, y=psi))+
  geom_text(aes(label=words), alpha=ifelse(weight$psi>1.5 | weight$beta>1.5 | weight$beta<(-.5), 1, .1))


weight$words[which(weight$beta<(-2))]


result <- wordfish.het(termMat, Nsamples=2000, beta.start=results$beta,
                       gamma.start=results$theta, alpha.start=results$alpha, psi.start=results$psi)



gammas <- result$gamma.samples
for(i in 1:nrow(gammas)) gammas[i,] <- (gammas[i,] - mean(gammas[i,]))/sd(gammas[i,])
gamma.res <- apply(gammas,2,mean)
gamma.res.se <- apply(gammas,2,sd)
r.res <- apply(1/result$r.samples,2,mean)
names(gamma.res) <- colnames(termMat)
names(r.res) <- colnames(termMat)

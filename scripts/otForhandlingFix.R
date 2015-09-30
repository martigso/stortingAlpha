rm(list=ls())

library(stortingAlpha)


path <- paste0("D:/ot_forhandlinger/",dir("D:/ot_forhandlinger"))


test <- unlist(lapply(path, function(x) paste0(x, "/", dir(x), "/")))


sapply(test, function(x) order.files(x, overwrite = TRUE))

sapply(test, function(x) collapse.text(x, overwrite = TRUE, lastCollapse = FALSE, chamber = "ot"))

sapply(path, function(x) collapse.text(x, overwrite = TRUE, lastCollapse = TRUE, chamber = "ot"))




year<-as.numeric(na.omit(as.numeric(unlist(strsplit(path, "[^0-9]+")))))

path <- gsub("/", "\\\\", path)

hm <- diag(sapply(path, function(x) paste0("move ", x, "\\o", year,  ".txt ",
                                           "C:\\Users\\Martin\\Dropbox\\Master\\Vitass\\Stortingsprosjektet\\Stortinget\\ot_forhandlinger\\Ot_forhandlinger", year, "\\")))

sapply(hm, function(x) shell(shQuote(x)))



x <- "'"

gsub("'", "hei", x)

string <- "[[:alpha:]]e[[:space:]][0-9]{1,3}[[:space:]]herrer|[[:alpha:]]e[[:space:]][0-9]{1,3}[[:space:]]repr[[:alpha:]]sentanter"
rv <- strapply(strapply(as.character(rcVote$text), string), "[0-9]{1,3}")

rcVote2 <- rcVote[which(sapply(rv, length)!=2),]

##Only for 1940
setwd("D:\\st_forhandlinger")
dir()
shell(shQuote(paste0("copy ", "*.txt ", "s", "1940", ".txt")))

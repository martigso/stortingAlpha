#' Scrap data on questions in the Storting
#'
#' A function to collect all questions asked by MPs to ministers in the Storting from 1996-1997.
#'
#' @param url String specifying one of the pages for the session to gather the questions from
#' @param nPages Integer specifying how many pages of questions the relevant session has (20 questions per page)
#'
#' @details Each session must be run seperately. All sessions can be combined together with \code{\link{rbind}}.
#'
#' @return Returns a data frame with one row for each question
#'
#'
#' @examples
#' url1314 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2013-2014&qtid=all&qsqid=all&page=1#list"
#' quest1314 <- parl.questions(url1314, nPages = 21)
#' quest1314$session <- "2013-2014"
#'
#' url1415 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2014-2015&qtid=all&qsqid=all&page=1#list"
#' quest1415 <- parl.questions(url1415, nPages = 19)
#' quest1415$session <- "2014-2015"
#'
#' @export
#'


parl.questions<-function(url, nPages){
  options(error=NULL)
  cases <- list()
  htmlTree <- list()
  typeString <- gsub("\\n", "", gsub("\\n[[:space:]]{1,50}", "", "Interpellasjon|Sp[[:alpha:]]rretimesp[[:alpha:]]rsm[[:alpha:]]l|
                     Skriftlig sp[[:alpha:]]rsm[[:alpha:]]l|Muntlig sp[[:alpha:]]rsm[[:alpha:]]l|
                                     Sp[[:alpha:]]rsm[[:alpha:]]l ved m[[:alpha:]]tets slutt|
                                     Sp[[:alpha:]]rsm[[:alpha:]]l til presidentskapet|Fra representanten"))

  url <- unlist(strsplit(gsub("([0-9]+)","~\\1~",url), "~" ))

  if(length(url)==3){
    for(i in 1:nPages){
      cases[i] <- paste0(url[1], i, url[3])
      htmlTree[i] <- getURL(cases[i], ssl.verifypeer=FALSE)
    }
  } else {
    for(i in 1:nPages){
      cases[i] <- paste0(url[1], url[2], url[3], url[4], url[5], i, url[7])
      htmlTree[i] <- getURL(cases[i], ssl.verifypeer=FALSE)
    }
  }

  htmlTree <- htmlTreeParse(htmlTree, useInternal = TRUE)

  htmlTree3 <- gsub("^\\r\\n[[:space:]]+|[[:space:]]+$", "", unlist(xpathApply(htmlTree, "//h3", xmlValue)))
  htmlTree4 <- gsub("^\\r\\n[[:space:]]+|[[:space:]]+$", "", unlist(xpathApply(htmlTree, "//h4", xmlValue)))

  if(length(htmlTree4)>=1122){
    htmlTree4[1122] <- gsub("Innlevert: 24.11.1997", "Sendt: 24.11.1997", as.character(htmlTree4[1122]))
  }

  if(any(grep("Dokument nr. 15:942 \\(2014-2015\\)", htmlTree4))==TRUE){
    htmlTree4 <- c(htmlTree4[1:1292], "Besvart: 06.05.2015 av finansminister Siv Jensen", htmlTree4[1293:length(htmlTree4)])
  }

  tempDate <- na.omit(
    htmlTree4[sapply(htmlTree4, function(x)
      grep("Til behandling|Bortfaller|Besvart:|Sp[[:alpha:]]rsm[[:alpha:]]let er trukket", x)==1)==TRUE])
  tempDate <- strapply(tempDate, "[0-9]{1,2}\\D[0-9]{1,2}\\D[0-9]{1,4}")

  tempAnswBy <- na.omit(
    htmlTree4[sapply(htmlTree4, function(x)
      grep("Til behandling|Bortfaller|Besvart:|Sp[[:alpha:]]rsm[[:alpha:]]let er trukket", x)==1)==TRUE])
  tempAnswBy <- unlist(strsplit(tempAnswBy, "^.*?minister |^.*?EU "))

  if(any(grep("Sp[[:alpha:]]rretimesp[[:alpha:]]rsm[[:alpha:]]l", htmlTree4))==FALSE){
    tempId <- na.omit(htmlTree4[sapply(htmlTree4, function(x) grep("Dokument nr. |Interpellasjon nr. ", x)==1)==TRUE])
    tempId <- gsub(" ", "", gsub("[[:punct:]]", "", sapply(strsplit(tempId, "Dokument nr. |Interpellasjon nr. "), "[[", 2)))
  } else {
    tempId <- rep(NA, length(htmlTree3))
  }


  temp<-data.frame(docId = tempId,
                   dateAsked = as.Date(gsub("Datert: |Sendt: ", "",
                                            na.omit(htmlTree4[sapply(htmlTree4, function(x)
                                              grep("Datert: |Sendt: ", x)==1)==TRUE])), "%d.%m.%Y"),
                   dateAnsw = as.Date(as.character(unlist(ifelse(lapply(tempDate, is.null)==TRUE, "Not answered", tempDate))),
                                      "%d.%m.%Y"),
                   type = unlist(strapply(htmlTree3, typeString)),
                   from = gsub(" \\(.*?$", "" , gsub("^.*?fra ", "", htmlTree3)),
                   answBy = tempAnswBy[which(nchar(tempAnswBy)>0)],
                   toDepMinister = gsub("^.*?til ", "", htmlTree3),
                   question = gsub("^\\r\\n[[:space:]]+|[[:space:]]+$", "", lapply(htmlTree["//div[@class='listitem-text']"], xmlValue)),
                   fromParty = gsub("\\).*?$", "" , gsub("^.*?\\(", "", htmlTree3)))

  return(temp)
}

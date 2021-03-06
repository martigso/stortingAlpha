#' Return a data frame of vote types based on text file
#'
#' A function to transform text into units of voteinfo
#'
#'
#' @param pattern Regex pattern to split the text file on
#' @param txtvector Vector of text to make into a data frame. Should be stored by the \code{\link{mend.text}} function.
#' @param strlength Integer indicating how much of the string after each vote should be kept. With rollcalls it is
#' seldom enough with under 1000
#'
#' @return Returns a dataframe with vote info on the specified text file
#'
#' @details If a C stack usage error occurs, this is often a consequence of infinite loops caused by wrongly specified pattern argument.
#' Check whether the pattern argument is correctly specified.
#'
#' @seealso \code{\link{percincrease}}
#'
#' @examples
#' order.files("~Storting/storting1899")
#' collapse.text("~Storting/storting1899")
#' s1899text <- mend.text("~Storting/storting1899/")
#' s1899df <- voteDf("Votering[[:punct:]]", s1899text, 500)
#'
#' @export
#'
votedf <- function(pattern, txtvector, strlength = 200){
  options(error=NULL)

  if(require(gsubfn)==FALSE){
    stop("Need to install package 'gsubfn'")
  }
  else {
    require(gsubfn)
  }
  require(stringi)
  year <- as.numeric(na.omit(as.numeric(unlist(strsplit(dir(), "[^0-9]+")))))
  preText <- lapply(txtvector, function(x) unlist(strsplit(as.character(x), pattern)))
    cat("Text split done...\n")

  nWordsTheme <- lapply(preText, function(x) stri_count_words(x[-length(x)]))
    cat("Word count complete...\n")

  postText <- lapply(txtvector, function(x) unlist(substring(as.list(unlist(strsplit(as.character(x), pattern)))[-1], 1, 2000)))
    cat("Post text done...\n")

  tempdf <- list()
  for(i in 1:length(txtvector)){
    tempdf[[i]] <-  data.frame(cbind(vote=as.character(unlist(strapply(txtvector[[i]], pattern))),
                                themeText=as.character(preText[[i]][-length(preText[[i]])]),
                                voteText=as.character(postText[[i]]),
                                nWordsTheme=as.numeric(nWordsTheme[[i]]),
                                totWords=as.numeric(stri_count_words(txtvector[[i]])),
                                year=year[i]))
                     cat(paste("Data frame", i, "of", length(txtvector), "done\n"))
  }

  tempdf <- do.call(rbind, tempdf)

  tempdf$enstem <- ifelse(grepl("enstemmig", tempdf$voteText)==TRUE | grepl("enst.", tempdf$voteText)==TRUE, 1, 0)
    cat("'enstem' done...\n")
  tempdf$motstem <- ifelse(grepl("m[[:alpha:]]d [[:digit:]]", tempdf$voteText)==TRUE | grepl("m[[:alpha:]]d[[:digit:]]", tempdf$voteText)==TRUE |
                           grepl("M[[:alpha:]]d [[:digit:]]", tempdf$voteText)==TRUE | grepl("M[[:alpha:]]d[[:digit:]]", tempdf$voteText)==TRUE |
                           grepl("m[[:alpha:]]t [[:digit:]]", tempdf$voteText) | grepl("m[[:alpha:]]t[[:digit:]]", tempdf$voteText)==TRUE |
                           grepl("M[[:alpha:]]t [[:digit:]]", tempdf$voteText)==TRUE | grepl("M[[:alpha:]]t[[:digit:]]", tempdf$voteText)==TRUE |
                           grepl("[[:alpha:]]kkebifaldt", tempdf$voteText)==TRUE |
                           grepl("[[:alpha:]]kke bifaldt", tempdf$voteText)==TRUE | grepl("[0-9]{1,3} stemmer", tempdf$voteText)==TRUE |
                           grepl("[0-9]{1,3} st[[:punct:]]", tempdf$voteText)==TRUE,
                         1,0)

  tempdf$rollcall <- ifelse(grepl("nanve[[:space:]]{0,1}opprop", tempdf$voteText)==TRUE | grepl("navne[[:space:]]{0,1}opraab", tempdf$voteText)==TRUE |
                            grepl("namn[[:space:]]{0,1}opprop", tempdf$voteText)==TRUE | grepl("navn[[:space:]]{0,1}oprop", tempdf$voteText)==TRUE |
                            grepl("namne[[:space:]]{0,1}upprop", tempdf$voteText)==TRUE | grepl("namne[[:space:]]{0,1}uprop", tempdf$voteText)==TRUE |
                            grepl("[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}herrer", tempdf$voteText)==TRUE |
                            grepl("[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}herrer", tempdf$voteText)==TRUE |
                            grepl("[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}w[[:space:]]{0,1}herrer[[:space:]]{0,1}var:", tempdf$voteText)==TRUE |
                            grepl("[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}repr[[:alpha:]]sentanter", tempdf$voteText)==TRUE |
                            grepl("[[:alpha:]]ei[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}repr[[:alpha:]]sentantane",tempdf$voteText)==TRUE|
                            grepl("[[:alpha:]]ei[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}repr[[:alpha:]]sentantar",tempdf$voteText)==TRUE,
                          1,0)
  cat("'rollcall' done...\n")
  tempdf$motstem <- ifelse(tempdf$motstem==1 & tempdf$rollcall==1, NA, tempdf$motstem)
  cat("'motstem' done...\n")
  tempdf$altvote <- ifelse(grepl("[[:alpha:]]lternativ votering", tempdf$voteText)==TRUE |
                             grepl("[[:alpha:]]lternativ voterig", tempdf$voteText)==TRUE |
                             grepl("[[:alpha:]]idere var indstillet", tempdf$voteText)==TRUE, 1, 0)

  #tempdf[,3:5] <- apply(tempdf[,3:5], 2, function(x) ifelse(tempdf$altvote==1, 0, x))
  cat("Function done...\n")
  return(tempdf)

}


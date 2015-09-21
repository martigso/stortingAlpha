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
votedf<-function(pattern, txtvector, strlength = 200){
  options(error=NULL)

  if(require(gsubfn)==FALSE){
    stop("Need to install package 'gsubfn'")
  }
  else {
    require(gsubfn)
  }

  tempdf<-data.frame(cbind(vote=unlist(strapply(txtvector, pattern)),
                           text=unlist(substring(as.list(unlist(strsplit(txtvector, pattern)))[-1], 1, strlength))))

  tempdf$enstem<-ifelse(grepl("enstemmig", tempdf$text)==TRUE | grepl("enst.", tempdf$text)==TRUE, 1, 0)

  tempdf$motstem<-ifelse(grepl("m[[:alpha:]]d [[:digit:]]", tempdf$text)==TRUE | grepl("m[[:alpha:]]d[[:digit:]]", tempdf$text)==TRUE |
                           grepl("M[[:alpha:]]d [[:digit:]]", tempdf$text)==TRUE | grepl("M[[:alpha:]]d[[:digit:]]", tempdf$text)==TRUE |
                           grepl("m[[:alpha:]]t [[:digit:]]", tempdf$text) | grepl("m[[:alpha:]]t[[:digit:]]", tempdf$text)==TRUE |
                           grepl("M[[:alpha:]]t [[:digit:]]", tempdf$text)==TRUE | grepl("M[[:alpha:]]t[[:digit:]]", tempdf$text)==TRUE |
                           grepl("[[:alpha:]]kkebifaldt", tempdf$text)==TRUE |
                           grepl("[[:alpha:]]kke bifaldt", tempdf$text)==TRUE | grepl("[0-9]{1,3} stemmer", tempdf$text)==TRUE |
                           grepl("[0-9]{1,3} st[[:punct:]]", tempdf$text)==TRUE,
                         1,0)

  tempdf$rollcall<-ifelse(grepl("nanveopprop", tempdf$text)==TRUE | grepl("navneopraab", tempdf$text)==TRUE |
                            grepl("namnopprop", tempdf$text)==TRUE | grepl("navnoprop", tempdf$text)==TRUE |
                            grepl("namneupprop", tempdf$text)==TRUE |
                            grepl("de[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}herrer", tempdf$text)==TRUE |
                            grepl("De[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}herrer", tempdf$text)==TRUE |
                            grepl("De[[:space:]][[:digit:]][[:space:]]w[[:space:]]herrer[[:space:]]var:", tempdf$text)==TRUE |
                            grepl("[[:alpha:]]e[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}repr[[:alpha:]]sentanter", tempdf$text)==TRUE |
                            grepl("[[:alpha:]]ei[[:space:]]{0,1}[0-9]{1,3}[[:space:]]{0,1}repr[[:alpha:]]sentantane",
                                  tempdf$text)==TRUE, 1,0)

  tempdf$motstem<-ifelse(tempdf$motstem==1 & tempdf$rollcall==1, 0, tempdf$motstem)

  tempdf$altvote <- ifelse(grepl("[[:alpha:]]lternativ votering", tempdf$text)==TRUE |
                             grepl("[[:alpha:]]lternativ voterig", tempdf$text)==TRUE |
                             grepl("[[:alpha:]]idere var indstillet", tempdf$text)==TRUE, 1, 0)

  #tempdf[,3:5] <- apply(tempdf[,3:5], 2, function(x) ifelse(tempdf$altvote==1, 0, x))

  return(tempdf)

}


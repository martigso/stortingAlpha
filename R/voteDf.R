#' Return a data frame of vote types based on text file
#'
#' A function to transform text into units of voteinfo
#'
#'
#' @param pattern Regex pattern to split the text file on
#' @param txtvector the text file to make into a data frame
#'
#' @return Returns a dataframe with vote info on the specified text file
#'
#' @seealso \code{\link{percincrease}}
#'
#' @examples
#' data<-voteDf("Votering[[:punct:]]", alltext)
#'
#' @export
#'
votedf<-function(pattern, txtvector){
  tempdf<-data.frame(cbind(vote=as.list(unlist(strapply(txtvector, pattern))),
                           text=substring(as.list(unlist(strsplit(txtvector, pattern))))[-1], 1, 500))

  tempdf$enstem<-ifelse(grepl("enstemmig", tempdf$text)==TRUE, 1, 0)
  tempdf$motstem<-ifelse(grepl("mot [[:digit:]]", tempdf$text)==TRUE | grepl("mod [[:digit:]]", tempdf$text)==TRUE |
                           grepl("mot[[:digit:]]", tempdf$text)==TRUE | grepl("mod[[:digit:]]", tempdf$text)==TRUE,
                         1,0)
  tempdf$rollcall<-ifelse(grepl("nanveopprop", tempdf$text)==TRUE | grepl("navneopraab", tempdf$text)==TRUE |
                            grepl("de [[:digit:]] herrer", tempdf$text)==TRUE | grepl("De [[:digit:]] herrer", tempdf$text)==TRUE, 1,0)

  tempdf

}

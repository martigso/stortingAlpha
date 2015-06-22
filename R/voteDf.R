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
votedf<-function(pattern, txtvector, strlength = 500){
  tempdf<-data.frame(cbind(vote=unlist(strapply(txtvector, pattern)),
                           text=unlist(substring(as.list(unlist(strsplit(txtvector, pattern)))[-1], 1, strlength))))

  tempdf$enstem<-ifelse(grepl("enstemmig", tempdf$text)==TRUE | grepl("enst.", tempdf$text)==TRUE, 1, 0)

  tempdf$motstem<-ifelse(grepl("m[[:alpha:]]d [[:digit:]]", tempdf$text) | grepl("m[[:alpha:]]d[[:digit:]]", tempdf$text)==TRUE |
                           grepl("M[[:alpha:]]d [[:digit:]]", tempdf$text)==TRUE | grepl("M[[:alpha:]]d[[:digit:]]", tempdf$text)==TRUE |
                           grepl("[[:alpha:]]lternativ votering", tempdf$text)==TRUE | grepl("[[:alpha:]]kkebifaldt", tempdf$text)==TRUE |
                           grepl("[[:alpha:]]kke bifaldt", tempdf$text)==TRUE,
                         1,0)
#  [[:alpha:]]ed [0-9]{1,2} stemmer |
#    [[:alpha:]]od [0-9]{1,2} stemmer |

    tempdf$motstem<-ifelse(tempdf$motstem==1 & grepl("de [0-9]{1,2} herrer", tempdf$text)==TRUE |
                           grepl("De [0-9]{1,2} herrer", tempdf$text)==TRUE, 0, tempdf$motstem)

  tempdf$rollcall<-ifelse(grepl("nanveopprop", tempdf$text)==TRUE | grepl("navneopraab", tempdf$text)==TRUE |
                            grepl("de [0-9]{1,2} herrer", tempdf$text)==TRUE | grepl("De [0-9]{1,2} herrer", tempdf$text)==TRUE, 1,0)

  tempdf

}

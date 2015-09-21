#' Data on votes in the Norwegian Storting from 1900 to 1940
#'
#' A dataset with voting descriptions, attributes and chamber the vote occured in
#'
#' @usage data("AllVotes")
#'
#' @format
#'  \describe{
#'  \item{vote}{The word the vote was detected by (all shuld be "Votering:")}
#'  \item{text}{A substring of 2000 characters after "Votering:" for each vote}
#'  \item{enstem}{A variable indicating whether the vote was unanimous (1) or not (0)}
#'  \item{motstem}{A variable indicating whether the vote was contested (1) or not (0)}
#'  \item{rollcall}{A variable indicating whether the vote was a roll call (1) or not (0)}
#'  \item{alvote}{A variable indicating whether the vote consisted of more than one vote (1) or not (0)}
#'  \item{year}{A variable indicating which year (session) the vote was held}
#'  \item{chamber}{A variable indicating whether the vote was held in the Storting, Odelsting, or Lagting}
#'
#'  }
#'
"AllVotes"

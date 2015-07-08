#' Data on parliamentary interpellations from 1996-2015 from the Norwegian Storting
#'
#' A dataset with key info on parliamentary interpellations from the Norwegian Storting (1996-2015).
#'
#' @usage data("interpAll")
#'
#' @format A data frame with 9902 rows and 11 variables
#'  \describe{
#'    \item{dateAsked}{Date interpellation was registered}
#'    \item{dateAnsw}{Date interpellation was answered (will only vary from \emph{dateAsked} when type is "Spørretimespørsmål)}
#'    \item{type}{Character string describing the type of interpellation}
#'    \item{from}{Character string with name of the interpellatorer}
#'    \item{fromParty}{Character string with party of interpellatorer}
#'    \item{answBy}{Character string of minister answering the interpellatation}
#'    \item{toDepMinister}{Character string with title of the minister the interpellation was asked to}
#'    \item{answByParty}{Character string of the answering minister's party}
#'    \item{nsd_id}{Integer with answering minister's id}
#'    \item{session}{Character string of the session the interpellation was asked and answered}
#'    \item{question}{Character string containing the full interpellation text}
#'    \item{party_id}{Integer with id of interpellationing party from ParlGov}
#'    \item{cabinet_party}{Binary variable indicating whether the party has cabinet position (1) or not (0)}
#'    \item{election_date}{Date of election the interpellation was asked under}
#'    \item{seats}{Integer with seats in parliament for interpellatorer}
#'    }
#'
#' @source \url{https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/}
#'
"interpAll"

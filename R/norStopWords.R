#' List of Norwegian stopwords
#'
#' Supplement to the tm-package on stopwords in NOrwegian
#'
#'
#' @return Returns list of Norwegian stopwords
#'
#'
#' @examples
#' norStop
#'
#' @export
#'

norStopWords <- function(...){
  options(error=NULL)

  if(require(tm)==FALSE){
    stop("Need to install package 'tm'")
  }  else {
    require(tm)
  }

    words <- c(stopwords("nor"), "aar", "adgang", "altsaa", "anden", "andet", "andre", "anledning", "beslutning",
               "bestemmelse", "blev", "blive", "bliver", "bør", "burde", "del", "derfor", "dog", "efter", "efterm",
               "end", "enig", "faa", "faar", "folk", "forhold", "forslag", "første", "frem", "gaa", "gaar", "ganske",
               "gjøre", "gjort", "godt", "grund", "have", "hele", "heller", "hensyn", "hvad", "ind", "kann", "komme",
               "kommer", "kunde", "lagtinget", "landet", "led", "lov", "loven", "maa", "mener", "mere", "mig", "naar",
               "ning", "nogen", "noget", "nok", "odelstinget", "odelstingets", "ogsaa", "ordet", "paa", "præsidenten",
               "punkt", "ret", "saa", "saadan", "saaledes", "sagt", "sak", "ser", "side", "sig", "sige", "skulde",
               "staar", "statsraad", "stemme", "stor", "store", "større", "synes", "tid", "tilfælde", "ting", "tror",
               "under", "været", "vedkommende", "vel", "vilde","all", "almind", "altsa", "and", "andr", "anmerkning",
               "ann", "arbeid", "bar", "behandling", "bemerkning", "bestemm", "bliv", "burd", "denn", "departement",
               "dett", "diss", "eft", "ell", "enkelt", "find", "følg", "forandr", "forandring", "form", "først", "gang",
               "gansk", "gjæld", "gjeld", "gjør", "hadd", "hav", "hel", "hell", "hold", "ikk", "ikkj", "ing", "komite",
               "komm", "kommun", "kong", "kund", "kunn", "lagting", "land", "lig", "ligg", "mang", "mening", "mer", "mul",
               "nog", "odelsting", "ogsa", "ord", "paragraf", "præsident", "president", "rett", "saal", "sætt", "sag",
               "samm", "sid", "sidst", "skuld", "slag", "spørsmaal", "stat", "sted", "stemm", "still", "størr", "syn",
               "tag", "tal", "tilfæld", "und", "uttal", "vær", "vedkomm", "vild", "virk")
    words
}



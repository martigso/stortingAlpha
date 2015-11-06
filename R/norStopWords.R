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

    words <- c(stopwords("nor"), "aar", "altsaa", "anden", "andet", "andre", "anledning","blev", "blive", "bliver",
               "b[[:alpha:]]r", "burde", "derfor", "dog", "efter", "efterm",
               "end", "faa", "faar", "f[[:alpha:]]rste", "frem", "gaa", "gaar", "ganske",
               "gj[[:alpha:]]re", "gjort", "godt", "grund", "have", "hele", "heller", "hvad", "ind", "kann", "komme",
               "kommer", "kunde", "led", "maa", "mener", "mere", "mig", "naar",
               "ning", "nogen", "noget", "nok", "ogsaa", "ordet", "paa", "pr[[:alpha:]]sidenten",
               "punkt", "saa", "saadan", "saaledes", "sagt", "sak", "ser", "side", "sig", "sige", "skulde",
               "staar", "stor", "store", "st[[:alpha:]]rre", "synes", "tid", "tilf[[:alpha:]]lde", "ting", "tror",
               "under", "v[[:alpha:]]ret", "vel", "vilde","all", "altsa", "and", "andr",
               "ann", "bar", "bliv", "burd", "denn", "dett", "diss", "eft", "ell", "enkelt", "find", "f[[:alpha:]]lg",
               "form", "f[[:alpha:]]rst", "gang", "gansk", "gj[[:alpha:]]r", "hadd", "hav", "hel", "hell", "hold", "ikk",
               "ikkj", "ing", "komm", "kong", "kund", "kunn", "lig", "ligg", "mang", "mening", "mer",
               "nog", "ogsa", "ord", "paragraf", "rett", "s[[:alpha:]]tt", "sag",
               "sid", "sidst", "skuld", "slag", "sted", "still", "syn",
               "tag", "tal", "tilf[[:alpha:]]ld", "und", "uttal", "v[[:alpha:]]r", "vild", "virk", "kap", "bifald", "bilfalt",
               "post", "hev", "tit", "gj[[:alpha:]]r", "titel", "indstilling", "indst", "pct", "vert")
    words
}

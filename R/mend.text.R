#' Mend full Storting text file before making vote data frame
#'
#' A function to mend misreadings of vote strings.
#'
#' @param path Character string of the directory where the full text file can be located.
#'
#' @details The function reads the full text file made by \code{\link{collapse.text}}, replaces misread
#' vote occurances using \code{\link{gsub}}, and splits the document into a data frame of these votes
#' using \code{\link{strapply}} from the \emph{gsubfn} package. It is important to run both
#' \code{\link{order.files}} and \code{\link{collapse.text}} before running this command.
#'
#' @return Returns one text vector where all single page text files are pasted in the right order.
#'
#' @seealso \code{\link{order.files}}, \code{\link{votedf}}, \code{\link{collapse.text}}
#'
#'
#' @export
#'

mend.text<-function(path){
  setwd(path)
  path<-getwd()
  year<-as.numeric(na.omit(as.numeric(unlist(strsplit(path, "[^0-9]+")))))
  textfile<-readLines(paste0("s", year, ".txt"), encoding="UTF-8")

  textfile<-gsub("-$", "", textfile)

  textfile<-paste(textfile, collapse="")

  # Strings of common misreadings to loop over
  strings<-"Votering:|V o t e r i n g :|V o t e r i n g:|V 0 t e r i n g:|
             V 0 t e r i n g :|votering:|Voteringz|Votering!|V o t e r i n g t|
             V 0 t e r in g :|V 0 t e ri n g:|V o t e ri n g:|V o t e ri n g :|
             V 0 t eir i n g :|Voterintg:|Votoring;|Voterin g:|V o t e. r i n g :|
             V o terinlg:|Voteringf|Voteri ng:|Votejring:|V 0|teri ngt|
             V 0 t de ri n g:|Vetering:|Voteiringz|Votüring:|Voteringzl|
             V 0 t e r i n g z|Voteriing:|v otering:|V 0 te ri n g:|V otering:|
             Vo,tering:|Vptering:|Vrotering:|V 0 t e  i n g:|V o tje r i n g:|
             Vjoterimg:|V o te r i n g :|V ote ring:|V o te ri n g:|V0te:ring:|
             V ote ri n g:|Vo t e r i n g :|Vg0 tzering:|V o te ring:|V o tering:|
             V oteri ng:|V o t e ring:|V o t e ring:|Vote-ring:|V 0 t e ri n g :|
             V 0 te r i n g :|V o t. e ri n g :|Vo-tering:|Votcring:|
             Vhojt e r i n g:|V o t e r i n g z|Vote ri-ng:|V ot e. ri n g :|
             Yotering:|Y otering.|Voterfi n g:|Vote ring:|Voteñing:|Vot ering:|
             V 0 te r i n g:|Vtfcering:|V o t e r i n g 2 .|Vot-ering:|Vntering:|
             Votering-z|V 0 t er i n g :|Xoterlng|Voterinrr:|Voteringi|
             Voteriiig:|Voteringr|V oteri n g:|Voteri n g:|Vcjtering:|Voterjng:|
             Voterirxg:|Votoring:|Volering:|V0 tu r i .n g:|Voterinv:|
             V 0 t e r i n g;|V 0 t e r i 11 g:|V 0 t e r i n g r|Vxitering:|
             V o t e r in g :|V oter ing:"

  textfile<-gsub(strings, "Votering:", textfile)

  strings<-"Enstemmig|en-stemmig|cnstennnig|onsteimni|existemmig|eirstemmig|
            nstemmig|ersstemtnig|Smithsstemmig|on-stemmig|en-sternmig|
            estemmig|onstonnnig|Enst.:|bifaldtes e-nst.|ensleirinnig|
            enlstemmig|enetetnznig|e11stem111ig|ei1sten1i;nig|entemmig|
            censlennnig"

  textfile<-gsub(strings, "enstemmig", textfile)

return(textfile)

}

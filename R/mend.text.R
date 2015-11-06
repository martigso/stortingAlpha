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
#' @examples
#' text <- mend.text("~Storting/storting1899/")
#'
#' @export
#'

mend.text <- function(path, chamber = "st"){
  options(error=NULL)
  setwd(path)
  path <- getwd()

  year <- as.numeric(na.omit(as.numeric(unlist(strsplit(dir(), "[^0-9]+")))))

  if(chamber == "st"){
    textfile  <- lapply(year, function(x) readLines(paste0("St_tidende", x, "\\s", x, ".txt"), encoding="UTF-8"))
      cat("Files loaded...\n")

    names(textfile) <- as.numeric(year)

    textfile <- lapply(textfile, function(x) paste(x, collapse=" "))
      cat("Collapse done...\n")

    textfile <- lapply(textfile, function(x) gsub("-[[:space:]]", "", x))
      cat("Line dash removed...\n")

    textfile <- lapply(textfile, function(x) gsub("'", "", x))

    # Strings of common misreadings to loop over
    strings <- "Votering:|Vote,r,lng:|V o t e r i n g :|V o t e r i n g:|V 0 t e r i n g:|V 0 t e r i n g :|Voterüng:|votering:|Voteringz|Votering!|V o t e r i n g t|V 0 t e r in g :|V 0 t e ri n g:|V o t e ri n g:|V o t e ri n g :|V 0 t eir i n g :|Voterintg:|Votoring;|Voterin g:|V o t e\\. r i n g :|V o terinlg:|Voteringf|Voteri ng:|Votejring:|V 0|teri ngt|V 0 t de ri n g:|Vetering:|Voteiringz|Voteringzl|V 0 t e r i n g z|Voteriing:|v otering:|V 0 te ri n g:|V otering:|Vo,tering:|Vptering:|Vrotering:|V 0 t e  i n g:|V o tje r i n g:|Vjoterimg:|V o te r i n g :|V ote ring:|V o te ri n g:|V0te:ring:|V ote ri n g:|Vo t e r i n g :|Vg0 tzering:|V o te ring:|V o tering:|V oteri ng:|V o t e ring:|V o t e ring:|Vote-ring:|V 0 t e ri n g :|V 0 te r i n g :|V o t\\. e ri n g :|Vo-tering:|Votcring:|Vhojt e r i n g:|V o t e r i n g z|Vote ri-ng:|V ot e\\. ri n g :|Yotering:|Y otering\\.|Voterfi n g:|Vote ring:|Voteñing:|Vot ering:|V 0 te r i n g:|Vtfcering:|V o t e r i n g 2 \\.|Vot-ering:|Vntering:|Votering-z|V 0 t er i n g :|Xoterlng|Voterinrr:|Voteringi|Voteriiig:|Voteringr|V oteri n g:|Voteri n g:|Vcjtering:|Voterjng:|Voterirxg:|Votoring:|Volering:|V0 tu r i \\.n g:|Voterinv:|V 0 t e r i n g;|V 0 t e r i 11 g:|V 0 t e r i n g r|Vxitering:|V o t e r in g :|V oter ing:|Voteri-ng:|Voterfing:|V o[']t e r i n g :|Voterting|Voteringc|Votericng:|Voterinz|V e t e ri ,n g :|Voterilng|Veterings|Voterlng:|Votering\\.|R[[:space:]]{0,1}[[:alpha:]][[:space:]]{0,1}y[[:space:]]{0,1}s[[:space:]]{0,1}t[[:space:]]{0,1}i[[:space:]]{0,1}n[[:space:]]{0,1}g[[:space:]]{0,1}:"

    textfile <- lapply(textfile, function(x) gsub(strings, "Votering:", x))
      cat("Misreadings of 'Votering' fixed...\n")

    strings <- "sarnr[[:alpha:]]ystes ved[[:space:]]{0,1}teken|sam[[:space:]]{0,1}r[[:alpha:]]ystes[[:space:]]{0,1}ved[[:space:]]{0,1}teken|Enstemmig|en-stemmig|cnstennnig|onsteimni|existemmig|eirstemmig|[[:space:]]nstemmig|ersstemtnig|Smithsstemmig|on-stemmig|en-sternmig|estemmig|onstonnnig|Enst\\.:|bifaldtes e-nst\\.|ensleirinnig|enlstemmig|enetetnznig|e11stem111ig|ei1sten1i;nig|entemmig|censlennnig|onst[[:punct:]]|enslonnmig|twist|Entemmig|euistennnig|ennstennmig|entem mig|ennstemnrig|eustem|len/sitemmig|endcemmigt|on[[:alnum:]]temmign|enptem|rnstennnig|ern stemrnig|en stemmig|len stemmig|en steznmig|en siemmig"

    textfile <- lapply(textfile, function(x) gsub(strings, "enstemmig", x))
      cat("Misreadings of 'enstemmig' fixed...\n")

    for(i in 1:length(textfile)){
      writeLines(as.character(textfile[i]), con = paste0("St_tidende", year[i], "\\St_tidende", year[i], "_red.txt"))
        cat(paste(year[i]), "written\n")
    }
  }

  if(chamber == "lt"){
      textfile  <- lapply(year, function(x) readLines(paste0("lt_forhandlinger", x, "\\l", x, ".txt"), encoding="UTF-8"))
      names(textfile) <- as.numeric(year)
      textfile <- lapply(textfile, function(x) paste(x, collapse=" "))

      textfile <- lapply(textfile, function(x) gsub("-[[:space:]]", "", x))

      textfile <- lapply(textfile, function(x) gsub("'", "", x))

      # Strings of common misreadings to loop over
      strings <- "Votering:|Vote,r,lng:|V o t e r i n g :|V o t e r i n g:|V 0 t e r i n g:|V 0 t e r i n g :|Voterüng:|votering:|Voteringz|Votering!|V o t e r i n g t|V 0 t e r in g :|V 0 t e ri n g:|V o t e ri n g:|V o t e ri n g :|V 0 t eir i n g :|Voterintg:|Votoring;|Voterin g:|V o t e\\. r i n g :|V o terinlg:|Voteringf|Voteri ng:|Votejring:|V 0|teri ngt|V 0 t de ri n g:|Vetering:|Voteiringz|Voteringzl|V 0 t e r i n g z|Voteriing:|v otering:|V 0 te ri n g:|V otering:|Vo,tering:|Vptering:|Vrotering:|V 0 t e  i n g:|V o tje r i n g:|Vjoterimg:|V o te r i n g :|V ote ring:|V o te ri n g:|V0te:ring:|V ote ri n g:|Vo t e r i n g :|Vg0 tzering:|V o te ring:|V o tering:|V oteri ng:|V o t e ring:|V o t e ring:|Vote-ring:|V 0 t e ri n g :|V 0 te r i n g :|V o t\\. e ri n g :|Vo-tering:|Votcring:|Vhojt e r i n g:|V o t e r i n g z|Vote ri-ng:|V ot e\\. ri n g :|Yotering:|Y otering\\.|Voterfi n g:|Vote ring:|Voteñing:|Vot ering:|V 0 te r i n g:|Vtfcering:|V o t e r i n g 2 \\.|Vot-ering:|Vntering:|Votering-z|V 0 t er i n g :|Xoterlng|Voterinrr:|Voteringi|Voteriiig:|Voteringr|V oteri n g:|Voteri n g:|Vcjtering:|Voterjng:|Voterirxg:|Votoring:|Volering:|V0 tu r i \\.n g:|Voterinv:|V 0 t e r i n g;|V 0 t e r i 11 g:|V 0 t e r i n g r|Vxitering:|V o t e r in g :|V oter ing:|Voteri-ng:|Voterfing:|V o[']t e r i n g :|Voterting|Voteringc|Votericng:|Voterinz|V e t e ri ,n g :|Voterilng|Veterings|Voterlng:|Votering\\.|R[[:space:]]{0,1}[[:alpha:]][[:space:]]{0,1}y[[:space:]]{0,1}s[[:space:]]{0,1}t[[:space:]]{0,1}i[[:space:]]{0,1}n[[:space:]]{0,1}g[[:space:]]{0,1}:"

      textfile <- lapply(textfile, function(x) gsub(strings, "Votering:", x))

      strings <- "sarnr[[:alpha:]]ystes ved[[:space:]]{0,1}teken|sam[[:space:]]{0,1}r[[:alpha:]]ystes[[:space:]]{0,1}ved[[:space:]]{0,1}teken|Enstemmig|en-stemmig|cnstennnig|onsteimni|existemmig|eirstemmig|[[:space:]]nstemmig|ersstemtnig|Smithsstemmig|on-stemmig|en-sternmig|estemmig|onstonnnig|Enst\\.:|bifaldtes e-nst\\.|ensleirinnig|enlstemmig|enetetnznig|e11stem111ig|ei1sten1i;nig|entemmig|censlennnig|onst[[:punct:]]|enslonnmig|twist|Entemmig|euistennnig|ennstennmig|entem mig|ennstemnrig|eustem|len/sitemmig|endcemmigt|on[[:alnum:]]temmign|enptem|rnstennnig|ern stemrnig|en stemmig|len stemmig|en steznmig|en siemmig"

      textfile <- lapply(textfile, function(x) gsub(strings, "enstemmig", x))

      for(i in 1:length(textfile)){
        writeLines(as.character(textfile[i]), con = paste0("lt_forhandlinger", year[i], "\\lt_forhandlinger", year[i], "_red.txt"))
      }
    }

    if(chamber == "ot"){
        textfile  <- lapply(year, function(x) readLines(paste0("ot_forhandlinger", x, "\\o", x, ".txt"), encoding="UTF-8"))
        names(textfile) <- as.numeric(year)
        textfile <- lapply(textfile, function(x) paste(x, collapse=" "))

        textfile <- lapply(textfile, function(x) gsub("-[[:space:]]", "", x))

        textfile <- lapply(textfile, function(x) gsub("'", "", x))

        # Strings of common misreadings to loop over
        strings <- "Votering:|Vote,r,lng:|V o t e r i n g :|V o t e r i n g:|V 0 t e r i n g:|V 0 t e r i n g :|Voterüng:|votering:|Voteringz|Votering!|V o t e r i n g t|V 0 t e r in g :|V 0 t e ri n g:|V o t e ri n g:|V o t e ri n g :|V 0 t eir i n g :|Voterintg:|Votoring;|Voterin g:|V o t e\\. r i n g :|V o terinlg:|Voteringf|Voteri ng:|Votejring:|V 0|teri ngt|V 0 t de ri n g:|Vetering:|Voteiringz|Voteringzl|V 0 t e r i n g z|Voteriing:|v otering:|V 0 te ri n g:|V otering:|Vo,tering:|Vptering:|Vrotering:|V 0 t e  i n g:|V o tje r i n g:|Vjoterimg:|V o te r i n g :|V ote ring:|V o te ri n g:|V0te:ring:|V ote ri n g:|Vo t e r i n g :|Vg0 tzering:|V o te ring:|V o tering:|V oteri ng:|V o t e ring:|V o t e ring:|Vote-ring:|V 0 t e ri n g :|V 0 te r i n g :|V o t\\. e ri n g :|Vo-tering:|Votcring:|Vhojt e r i n g:|V o t e r i n g z|Vote ri-ng:|V ot e\\. ri n g :|Yotering:|Y otering\\.|Voterfi n g:|Vote ring:|Voteñing:|Vot ering:|V 0 te r i n g:|Vtfcering:|V o t e r i n g 2 \\.|Vot-ering:|Vntering:|Votering-z|V 0 t er i n g :|Xoterlng|Voterinrr:|Voteringi|Voteriiig:|Voteringr|V oteri n g:|Voteri n g:|Vcjtering:|Voterjng:|Voterirxg:|Votoring:|Volering:|V0 tu r i \\.n g:|Voterinv:|V 0 t e r i n g;|V 0 t e r i 11 g:|V 0 t e r i n g r|Vxitering:|V o t e r in g :|V oter ing:|Voteri-ng:|Voterfing:|V o[']t e r i n g :|Voterting|Voteringc|Votericng:|Voterinz|V e t e ri ,n g :|Voterilng|Veterings|Voterlng:|Votering\\.|R[[:space:]]{0,1}[[:alpha:]][[:space:]]{0,1}y[[:space:]]{0,1}s[[:space:]]{0,1}t[[:space:]]{0,1}i[[:space:]]{0,1}n[[:space:]]{0,1}g[[:space:]]{0,1}:"

        textfile <- lapply(textfile, function(x) gsub(strings, "Votering:", x))

        strings <- "sarnr[[:alpha:]]ystes ved[[:space:]]{0,1}teken|sam[[:space:]]{0,1}r[[:alpha:]]ystes[[:space:]]{0,1}ved[[:space:]]{0,1}teken|Enstemmig|en-stemmig|cnstennnig|onsteimni|existemmig|eirstemmig|[[:space:]]nstemmig|ersstemtnig|Smithsstemmig|on-stemmig|en-sternmig|estemmig|onstonnnig|Enst\\.:|bifaldtes e-nst\\.|ensleirinnig|enlstemmig|enetetnznig|e11stem111ig|ei1sten1i;nig|entemmig|censlennnig|onst[[:punct:]]|enslonnmig|twist|Entemmig|euistennnig|ennstennmig|entem mig|ennstemnrig|eustem|len/sitemmig|endcemmigt|on[[:alnum:]]temmign|enptem|rnstennnig|ern stemrnig|en stemmig|len stemmig|en steznmig|en siemmig"

        textfile <- lapply(textfile, function(x) gsub(strings, "enstemmig", x))

        for(i in 1:length(textfile)){
          writeLines(as.character(textfile[i]), con = paste0("ot_forhandlinger", year[i], "\\ot_forhandlinger", year[i], "_red.txt"))
        }
      }
    }


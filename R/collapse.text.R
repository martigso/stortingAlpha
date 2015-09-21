#' Construct text document on single page text files
#'
#' A function to collapse single page text files into one text file using a combination of R and shell commands.
#'
#' @param path String specifying the directory containing the single page text files
#' @param overwrite Logical. If there already is a complete text file in the directory, should it be overwritten?
#'
#' @details It is important to make sure that the files are propperly arranged using the \code{\link{order.files}}
#' command before running \emph{collapse.text}.
#' Only works on Windows OS for the time being.
#'
#' @return Returns a list of the files copied in the console, and a string of what the file is called and where to locate it.
#'
#' @seealso \code{\link{order.files}}, \code{\link{votedf}}
#'
#' @examples
#' collapse.text("~Storting/storting1899/")
#'
#' @export
#'

collapse.text<-function(path, overwrite = TRUE, lastCollapse = FALSE, chamber = "st"){
  options(error = NULL)

  year<-as.numeric(na.omit(as.numeric(unlist(strsplit(path, "[^0-9]+")))))
  letter <- tail(unlist(strsplit(path, "\\/")), 1)

  if(Sys.info()[1]!="Windows"){
    stop("This command only works on Windows OS")
  }


  #if(paste0("o", year, ".txt") %in% list.files(path)==TRUE & overwrite==FALSE){
   # stop("Overwrite is set to false, and there is already a full text file in the directory.")
  #  }
  if(chamber == "st"){
    if(lastCollapse==FALSE){
      setwd(path)

      shell(shQuote(paste0("copy *.txt ", letter, year, ".txt")))

      path <- gsub("/", "\\\\", path)

      shell(shQuote(paste0("move ", path, letter, year,  ".txt ", substring(path, 1, 35))))

      }

    if(lastCollapse == TRUE){

      path <- unique(substring(path, 1, 35))

      setwd(path)

      shell(shQuote(paste0("copy ", "*.txt ", "s", year, ".txt")))
    }
  }

  if(chamber == "lt"){
    if(lastCollapse==FALSE){
      setwd(path)

      shell(shQuote(paste0("copy *.txt ", letter, year, ".txt")))

      path <- gsub("/", "\\\\", path)

      shell(shQuote(paste0("move ", path, letter, year,  ".txt ", substring(path, 1, 40))))

    }

    if(lastCollapse == TRUE){

      path <- unique(substring(path, 1, 40))

      setwd(path)

      shell(shQuote(paste0("copy ", "*.txt ", "l", year, ".txt")))
    }
  }

  if(chamber == "ot"){
    if(lastCollapse==FALSE){
      setwd(path)

      shell(shQuote(paste0("copy *.txt ", letter, year, ".txt")))

      path <- gsub("/", "\\\\", path)

      shell(shQuote(paste0("move ", path, letter, year,  ".txt ", substring(path, 1, 40))))

    }

    if(lastCollapse == TRUE){

      path <- unique(substring(path, 1, 40))

      setwd(path)

      shell(shQuote(paste0("copy ", "*.txt ", "o", year, ".txt")))
    }
  }
}

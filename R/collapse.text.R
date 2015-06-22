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
#' @export
#'

collapse.text<-function(path, overwrite=TRUE){
  options(error=NULL)

  if(Sys.info()[1]!="Windows"){
    stop("This command only works on Windows OS")
  }

  setwd(path)
  path<-getwd()
  year<-as.numeric(na.omit(as.numeric(unlist(strsplit(path, "[^0-9]+")))))

  if(paste0("s", year, ".txt") %in% list.files(path)==TRUE & overwrite==FALSE){
    stop("Overwrite is set to false, and there is already a full text file in the directory.")
    } else {
      shell(shQuote(paste0("copy /Y *.txt ", "s", year, ".txt")))
    }
  return(paste0("The file s", year, ".txt was successfully made in ", path, "."))
  }

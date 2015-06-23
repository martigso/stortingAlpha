#' Change file names on text files
#'
#' The \code{order.files} function renames the .txt-files in the specified directory by adding two zeroes to single
#' digits and one zero to double digit numbers within file names.
#'
#'
#' @param path character vector or string specifying the path to rename files from (remember to include two backslashes or one slash, depending on which you use, at the end of the string).
#'
#' @return Either returs a statement of success, or a warning when it fails.
#'
#'
#' @examples
#' order.files("~Storting/storting1899/Text/")
#'
#' @export
#'

order.files<-function(path, overwrite=TRUE){
  options(error=NULL)

  if(Sys.info()[1]!="Windows"){
    stop("This command only works on Windows OS")
  }

  files<-list.files(path, pattern=".txt")
  filesnew<-as.numeric(na.omit(as.numeric(unlist(strsplit(unlist(files), "[^0-9]+")))))

  if(overwrite==FALSE & all(nchar(files)==8) | any(nchar(files))==9){
    stop("Overwrite set to false, and the files are missing, already renamed,
         a full text document is already made, or the directory does not exist")
  }

  if(overwrite==TRUE | overwrite==FALSE & length(files)>0){
      filesnew<-ifelse(nchar(as.character(filesnew))==1, paste0("00", as.character(filesnew)), filesnew)
      filesnew<-ifelse(nchar(as.character(filesnew))==2, paste0("0", as.character(filesnew)), filesnew)
      filesnew<-paste0(substring(files, 1, 1), filesnew, ".txt")
      }

  filesnew<-paste0(path, filesnew)
  files<-paste0(path, files)
  doneLogic<-file.rename(files, filesnew)

  if (all(doneLogic)==TRUE){
    return("File names changed")
  } else {
    stop("Something went wrong! Did you remember to add '\\\\' or '/' at the end of the path string?
         Else, make sure directory name is correctly specified.")
  }

}





















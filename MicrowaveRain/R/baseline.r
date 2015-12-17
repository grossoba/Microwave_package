# A Cat Function
#'
#' This function allows you to open a data file
#' it has to be a textfile with the date/time in the first column and the intensity in the second one
#' @param
#' @keywords
#' @export
#' @examples
#' Import_Data()
#'

#############################
## IMPORT DATA FROM A FILE ##
#############################
Import_Data <- function(){
  print("Choose your data file")
  file_path <- file.choose()
  mydata = read.table(file_path,header = TRUE,sep = ";")
  return(mydata)
}
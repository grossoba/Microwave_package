# A Cat Function
#'
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
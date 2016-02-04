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
# The input vector is the difference between the baseline and the attenuation
# the positive values are the rain (remember that attenuation is negative)
# the conversion function below sets to zero all the negative values (dry event)
Convert_into_rain <- function(attenuation,cste_k_a){
  
  rain <- array(0,length(attenuation))
  
  replace(attenuation,attenuation<0,0)
  
  rain <- (attenuation/cste_k_a[1])**(1/cste_k_a[2])
  
  return(rain)
}
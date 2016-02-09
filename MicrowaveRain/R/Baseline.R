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
Bsline_mean_value <- function(attenuation){

  bsline <- mean(attenuation[,2])
  bsline_attenuation <- array(0,c(length(attenuation[,2]),2))
  bsline_attenuation[,1] <- attenuation[,1]
  
  bsline_attenuation[,2] <- bsline - attenuation[,2]
  
  return(bsline_attenuation)
}

###################
## MOVING WINDOW ##
###################
Moving_window <- function(size,data)
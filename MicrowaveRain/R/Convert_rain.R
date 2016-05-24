# A Cat Function
#'
#' @param
#' @keywords
#' @export
#' @examples
#' Import_Data()
#'

############################################
## CONVERT ATTENUATION INTO RAINFALL RATE ##
############################################
# The input vector is the difference between the baseline and the attenuation
# the positive values are the rain (remember that attenuation is negative)
# the conversion function below sets to zero all the negative values (dry event)
Convert_into_rain <- function(attenuation,cste_k_a){
  
  rain <- array(0,c(length(attenuation[,2]),2))
  rain[,1] <- attenuation[,1]
  
  attenuation[as.numeric(attenuation[,2])<0,2] = 0
  
  rain[,2] <- (as.numeric(attenuation[,2])/cste_k_a[1])**(1/cste_k_a[2])
  
  return(rain)
}

###################################################
## CONSTANTS AND FREQUENCIES FOR ANTENNAS        ##
###################################################
# Function that takes the frequency and the polarization
# and returns the two corresponding constants k and alpha
# from the ITU-R table P.838-3
###################################################
Antenna_freq <-function(freq, polari)
{
  cste_antenna <- array(0,2)
  mydata <- read.table("/home/dwhtest/Microwave_package/MicrowaveRain/Data/ITU-R",header = TRUE,sep = ";",stringsAsFactors=FALSE)
  
  index <- which.min(abs(mydata[,1]-freq))
  
  if(polari =="H")
  {
    cste_antenna <- c(mydata[index,2], mydata[index,3])
  }
  else if(polari =="V")
  {
    cste_antenna <- c(mydata[index,4], mydata[index,5])
  }
  else
  {
    print("Error : wrong polarization")
  }
  
  return(cste_antenna)
}
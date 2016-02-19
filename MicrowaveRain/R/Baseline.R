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
# It computes the attenuation between the signal received and a 
# baseline given by the mean of the sample
Bsline_meanValue <- function(attenuation){

  bsline <- mean(as.numeric(attenuation[,2]))
  bsline_attenuation <- array(0,c(length(attenuation[,2]),2))
  bsline_attenuation[,1] <- attenuation[,1]
  
  bsline_attenuation[,2] <- bsline - as.numeric(attenuation[,2])
  
  return(bsline_attenuation)
}

##########################
## MOVING WINDOW STATIC ##
##########################
# The moving window works on time interval and not on number of elements
# One gives the width of the interval in min and an attenuation vector (time and attenuation)
# The function computes for each interval the quantity given by f on each interval of size "width"
# The third column of the returned vector contains the computed local quantity given par f(...)
Moving_window_static <- function(attenuation,width,f)
{
  size_data <- as.numeric(difftime(strptime(attenuation[length(attenuation[,2]),1],"%Y-%m-%d %H:%M:%S"),strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S"),units="mins")) # size of the data in min
  nbr_intervals <- floor(size_data/width) # number of intervals not considering the rest of division
  mvingwdw_attenuation <- array(0,c(length(attenuation[,2]),3))

  mvingwdw_attenuation[,1] <- attenuation[,1]
  mvingwdw_attenuation[,2] <- attenuation[,2]
  
  mean_value <- 0
  for(i in 1:(nbr_intervals+1))
    {
    # mean value in an interval [t0,t0+width] in min given by "width"
    mean_value <- f(as.numeric(attenuation[(strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
                  strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width),2]))

    # mean value on the interval for all the elements of this interval
    mvingwdw_attenuation[strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
               strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width,3] <- mean_value
  
    # remove the baseline (mean value on the interval) from all the elements of the interval
#     bsline_attenuation[strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
#                   strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width,2] <- as.numeric(mean_value) - as.numeric(attenuation[strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
#      strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width,2])
  }

  return(mvingwdw_attenuation)
}

###########################
## MOVING WINDOW DYNAMIC ##
###########################
# The moving window works on time interval and not on number of elements
# One gives the width of the interval in min and an attenuation vector (time and attenuation)
# The function computes for each interval the quantity given by f on each interval of size "width"
# The third column of the returned vector contains the computed local quantity given par f(...)
Moving_window_dyn <- function(attenuation,width,f)
{
  size_data <- as.numeric(difftime(strptime(attenuation[length(attenuation[,2]),1],"%Y-%m-%d %H:%M:%S"),strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S"),units="mins")) # size of the data in min
  nbr_intervals <- floor(size_data/width) # number of intervals not considering the rest of division
  mvingwdw_attenuation <- array(0,c(length(attenuation[,2]),3))
  
  mvingwdw_attenuation[,1] <- attenuation[,1]
  mvingwdw_attenuation[,2] <- attenuation[,2]
  
  mean_value <- 0
  for(i in 1:(length(attenuation[,2])))
  {
    # mean value in an interval [t0,t0+width] in min given by "width"
    mean_value <- f(as.numeric(attenuation[(strptime(attenuation[i,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
                                              strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[i,1],"%Y-%m-%d %H:%M:%S") + 60*width),2]))
    
    # mean value on the interval for all the elements of this interval
    mvingwdw_attenuation[i,3] <- mean_value
    
    # remove the baseline (mean value on the interval) from all the elements of the interval
    #     bsline_attenuation[strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
    #                   strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width,2] <- as.numeric(mean_value) - as.numeric(attenuation[strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S")+(i-1)*60*width <= strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") & 
    #      strptime(attenuation[,1],"%Y-%m-%d %H:%M:%S") <= strptime(attenuation[1,1],"%Y-%m-%d %H:%M:%S") + i*60*width,2])
  }
  
  return(mvingwdw_attenuation)
}



Schleiss_window <- function()
{
  
  
  
  
}




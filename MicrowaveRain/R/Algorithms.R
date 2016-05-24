# A Cat Function
#'
#' @param
#' @keywords
#' @export
#' @examples
#' Import_Data()
#'


##############################
## Schleiss Decision rule 1 ##
##############################
# This functions calls the Moving_window_dyn function and receives in the mvingwdw_attenuation vector [time,attenuation,mean,sd]
# According to the standard deviation the rainy and dry events are detected and the baseline is computed
# The returned vector is [time, attenuation with baseline removed,attenuation, mean for each point on the interval defined]
Schleiss_S1 <- function(attenuation,width,threshold)
{
  mvingwdw_attenuation <- array(0,c(length(attenuation[,2]),length(fcts) + 2))
  fcts <- list(function(x) mean(x), function(x) sd(x)) 
  mvingwdw_attenuation <- Moving_window_dyn(attenuation,width,fcts,"asymm")
  
  schleiss_attenuation <- array(0,c(length(attenuation[,2]),4))
  
  schleiss_attenuation[,1] <- attenuation[,1]
  schleiss_attenuation[,2] <- mvingwdw_attenuation[,4] # sd --> attenuation - baseline
  schleiss_attenuation[,3] <- attenuation[,2] # attenuation received
  schleiss_attenuation[,4] <- mvingwdw_attenuation[,3] # mean --> baseline
  
  
  schleiss_attenuation[mvingwdw_attenuation[,4] <= threshold,2] <- as.numeric(mvingwdw_attenuation[mvingwdw_attenuation[,4] <= threshold,3])
  -as.numeric(attenuation[mvingwdw_attenuation[,4] <= threshold,2]) # Dry
  
  print(mvingwdw_attenuation[mvingwdw_attenuation[,4] > threshold,4])
  ind_nan <- which(mvingwdw_attenuation[,4] > threshold)
  diff_ind_nan <- diff(ind_nan)
  xdiff_ind_nan <- which(diff_ind_nan > 1)
  
  #   print(ind_nan)
  #   print(diff_ind_nan)
  #   print(xdiff_ind_nan)
  #   print(diff_ind_nan[xdiff_ind_nan])
  #   print(ind_nan[xdiff_ind_nan+1])
  
  schleiss_attenuation[ind_nan[xdiff_ind_nan+1],2] <- as.numeric(mvingwdw_attenuation[ind_nan[xdiff_ind_nan],3]) 
  - as.numeric(attenuation[ind_nan[xdiff_ind_nan+1],2])
  
  print(schleiss_attenuation[ind_nan[xdiff_ind_nan+1],2])
  
  # so far ok...
  for(i in 1:length(xdiff_ind_nan))
  {
    if(i == 1)
    {
      schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1],2] <- as.numeric(schleiss_attenuation[(ind_nan[1]-1),4]) - as.numeric(schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1],3])
      schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1],4] <- as.numeric(schleiss_attenuation[(ind_nan[1]-1),4]) 
      
    }
    else if(i == length(xdiff_ind_nan))
    {
      schleiss_attenuation[ind_nan > ind_nan[xdiff_ind_nan[i]+1],2] <- as.numeric(schleiss_attenuation[(ind_nan[xdiff_ind_nan[i]]-1),4]) - as.numeric(schleiss_attenuation[ind_nan > ind_nan[xdiff_ind_nan[i]+1],3])
      schleiss_attenuation[ind_nan > ind_nan[xdiff_ind_nan[i]+1],4] <- as.numeric(schleiss_attenuation[(ind_nan[xdiff_ind_nan[i]]-1),4])
      
    }
    else
    {
      schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1] & ind_nan > ind_nan[xdiff_ind_nan[i-1]+1],2] <- as.numeric(schleiss_attenuation[ind_nan[xdiff_ind_nan[i]]-1,4]) - as.numeric(schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1] & ind_nan > ind_nan[xdiff_ind_nan[i-1]+1],3])
      schleiss_attenuation[ind_nan < ind_nan[xdiff_ind_nan[i]+1] & ind_nan > ind_nan[xdiff_ind_nan[i-1]+1],4] <- as.numeric(schleiss_attenuation[ind_nan[xdiff_ind_nan[i]]-1,4])
      
    }
  }
  
  
  
  
  return(schleiss_attenuation)
}

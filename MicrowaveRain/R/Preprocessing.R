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
Import_Data <- function(){
  print("Choose your data file")
  file_path <- file.choose()
  mydata = read.table(file_path,header = TRUE,sep = ";")
  return(mydata)
}

# This functions takes as input a table with in the first column the intensity and in the second one
# the intensity. First it splits the lines between intervals given by "the time_unit" and "dt" input
# variables. It returns a table with average intensity and last time value of the interval
Average_Dates <- function(Intensity,time_unit,dt){
 intervals <- endpoints(Intensity[,1],on=time_unit,dt)
 Intensity_avg <- matrix(0,nrow = (length(intervals)-1), ncol = 2)

 for(i in 1:(length(intervals)-1))
 {
   sum <- 0
   for(j in (intervals[i]+1):(intervals[i+1]))
   {
   sum <- sum + Intensity[j,2]
   }

   Intensity_avg[i,1] <- as.character(Intensity[(intervals[i+1]),1])
   Intensity_avg[i,2] <- sum/(intervals[i+1]-intervals[i])
 }
 return(Intensity_avg)
}


Check_NA <- function(Intensity){
 NA.index <- which(is.na(Intensity[,2]))

 for(i in NA.index){
   if(is.na(Intensity[i,2])){
     if(!is.na(Intensity[i-1,2])){
       if(!is.na(Intensity[i+1,2])){
         Intensity[i,2] <- mean(Intensity[i+1,2],Intensity[i-1,2])
       }
       if(is.na(Intensity[i+1,2]) & !is.na(Intensity[i+2,2])){
         Intensity[i,2] <- mean(Intensity[i+2,2],Intensity[i-1,2])
         Intensity[i+1,2] <- Intensity[i,2]
       }
     }
   }
 }
return(Intensity)
}

Time_diff <- function(Intensity,time_unit)
{
  Intensity_diff <- matrix(0,nrow = (length(Intensity[,1])), ncol = 2)

  for(i in 1:length(Intensity[,1]))
  {
  Intensity_diff[i,1] <- as.numeric(difftime(myIntensity[i,1],myIntensity[1,1], units=c(time_unit)))
  Intensity_diff[i,2] <- Intensity[i,2]

  }
  return(Intensity_diff)


}

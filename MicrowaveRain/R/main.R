#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

# install.packages("xts")
# library(xts)

#########################################################################
##                                                                     ##
##    Importation of the data, check and scaling of the time           ##
##                                                                     ##
#########################################################################
print("Choose the main.R file")
FILE_path <- file.choose()
DIR_path <- dirname(FILE_path)
setwd(DIR_path)
source("Preprocessing.R")


myMicrowave <- Import_Data()
myIntensity <-Import_Data()

# myIntensitytime = paste(myIntensity$date,myIntensity$time) # gather date and time
# myIntensitytime
# myIntensity$date = strptime(myIntensitytime,"%Y-%m-%d %H:%M:%S")
myIntensity$date = strptime(myIntensity$date,"%Y-%m-%d %H:%M:%S")
# myIntensity <- myIntensity[,-2] # removes the "time" column as date and time were gathered

myIntensity <- Check_NA(myIntensity)

myIntensity_avg <- Average_Dates(myIntensity,"mins",1)


myIntensity_diff <- Time_diff(myIntensity_avg,"mins")
# plot(myIntensity_avg[,2],type="n",ylim=as.numeric(c(min(myIntensity_avg[,2]),max(myIntensity_avg[,2]))))
plot(myIntensity_diff[,1],myIntensity_diff[,2],type="n",ylim=(c(-50.4,-50.8)))
points(myIntensity_diff[,1],myIntensity_diff[,2],type="l",pch = ".")


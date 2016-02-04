#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

# if(!require("xts")) install.packages("xts", repos="http://cran.rstudio.com/")$
# 
# if(!require("RPostgreSQL")) install.packages("RPostgreSQL", repos="http://cran.rstudio.com/")$
# if(!require("DBI")) install.packages("DBI", repos="http://cran.rstudio.com/")$
library(xts)
library(RPostgreSQL)
library("date")
library("chron")

#########################################################################
##                                                                     ##
##    Importation of the data, check and scaling of the time           ##
##                                                                     ##
#########################################################################
print("Choose the main.R file")
# FILE_path <- file.choose()
FILE_path <- "/home/dwhtest/Microwave_package/MicrowaveRain/R/main.R"
DIR_path <- dirname(FILE_path)
setwd(DIR_path)
source("Preprocessing.R")
source("Convert_rain.R")

myMicrowave <- Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/links2simulation_ev3_out.csv")
dataAttenuation <-Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/ZH7527A_ZH0027C.txt")
dataRain <- Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/ZH7527A_ZH0027C.txt")


dataAttenuation$date = strptime(dataAttenuation$date,"%Y-%m-%d %H:%M:%S")
# dataRain$date = strptime(dataRain$date,"%Y-%m-%d %H:%M:%S")

dataAttenuation <- Check_NA(dataAttenuation)
# dataRain <- Check_NA(dataRain)

dataAttenuation <- Average_Dates(dataAttenuation,"mins",1)
# dataRain <- Average_Dates(dataRain,"mins",1)

dataAttenuation_aggregate <- Aggregate_data(dataAttenuation,10,"%Y-%m-%d %H:%M")
dataAttenuation_aggregate <- Complete_data(dataAttenuation_aggregate,2)
# dataRain_aggregate <- Aggregate_data(dataRain,10,"%Y-%m-%d %H:%M")
# dataRain_aggregate <- Complete_data(dataRain_aggregate,2)


# par(mfrow = c(2, 2))
dataAttenuation_diff <- Time_diff(dataAttenuation,"mins")
# Draw_plot(dataAttenuation_diff,1,2,"Time [min]", "Attenuation [dB]")
# 
# dataRain_diff <- Time_diff(dataRain,"mins")
# Draw_plot(dataRain_diff,1,2,"Time [min]", "Rainfall rate [mm/h]")




# plot(dataRain_diff[,1],dataRain_diff[,2],type="n")
# points(dataRain_diff[,1],dataRain_diff[,2],type="l",pch = ".")
# 
# plot(dataRain_aggregate[,1],dataRain_aggregate[,2],type="n")
# points(dataRain_aggregate[,1],dataRain_aggregate[,2],type="l",pch = ".")


mode(dataAttenuation_diff) <- "numeric"


cste_k_a <- Antenna_freq(58,"V")
dataRain <- Convert_into_rain(abs(dataAttenuation_diff[,2]),cste_k_a)

par(mfrow = c(2,1))
plot(dataAttenuation_diff[,1],dataAttenuation_diff[,2],type="n")
points(dataAttenuation_diff[,1],dataAttenuation_diff[,2],type="l",pch = ".")

plot(dataAttenuation_diff[,1],dataRain,type="n")
points(dataAttenuation_diff[,1],dataRain,type="l",pch = ".")


#######################
# FOURIER ATTEMPT     #
#######################
# max_hf <- 8
# high_freq2 <- array(0,c(length(dataRain_diff[,2])))
# data_Rain_fft <- fft(as.numeric(dataRain_diff[,2]), inverse = FALSE)
# high_freq1 <- data_Rain_fft[abs(data_Rain_fft) > max_hf ] # doesn't work with 2 tests ?!
# 
# for(i in 1:length(dataRain_diff[,2])){
#   if(abs(data_Rain_fft[i]) > max_hf){
#     high_freq2[i] <- dataRain_diff[i,2]
#   }
#   else{
#     high_freq2[i] <- NaN
#   }
# }
# inv_high_freq1 <- fft(high_freq1, inverse = TRUE)
# 
# par(mfrow = c(2, 2))
# plot(dataRain_diff[,1],dataRain_diff[,2],type="n")
# points(dataRain_diff[,1],dataRain_diff[,2],type="l",pch = ".")
# 
# plot(dataRain_diff[,1],high_freq2,type="n")
# points(dataRain_diff[,1],high_freq2,type="l",pch = ".")
# plot(as.ts(inv_high_freq1), type = 'l', pch=".")
# plot(high_freq1,type="p",pch="x", xlim=c(-100,100),ylim=c(-100,100))



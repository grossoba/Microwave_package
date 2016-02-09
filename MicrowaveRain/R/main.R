#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

###############
## LIBRARIES ##
###############
library(xts)
library(RPostgreSQL)
library("date")
library("chron")

#################
## IMPORT DATA ##
#################
print("Choose the main.R file")
FILE_path <- "/home/dwhtest/Microwave_package/MicrowaveRain/R/main.R"
DIR_path <- dirname(FILE_path)
setwd(DIR_path)
source("Preprocessing.R")
source("Convert_rain.R")
source("Baseline.R")

myMicrowave <- Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/links2simulation_ev3_out.csv")
dataAttenuation <-Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/ZH7527A_ZH0027C.txt")
dataRain <- Import_Data("/home/dwhtest/Microwave_package/MicrowaveRain/Data/ZH7527A_ZH0027C.txt")


###################
## PREPROCESSING ##
###################
dataAttenuation$date = strptime(dataAttenuation$date,"%Y-%m-%d %H:%M:%S")
dataAttenuation <- Check_NA(dataAttenuation)
dataAttenuation <- Average_Dates(dataAttenuation,"mins",1)
dataAttenuation_aggregate <- Aggregate_data(dataAttenuation,10,"%Y-%m-%d %H:%M")
dataAttenuation_aggregate <- Complete_data(dataAttenuation_aggregate,2)
dataAttenuation_diff <- Time_diff(dataAttenuation,"mins")
mode(dataAttenuation_diff) <- "numeric"

##############
## BASELINE ##
##############
bsln_meanAttenuation <- Bsline_mean_value(dataAttenuation_diff)

#####################
## RAIN CONVERSION ##
#####################
cste_k_a <- Antenna_freq(58,"V")
dataRain <- Convert_into_rain(bsln_meanAttenuation,cste_k_a)


###########
## PLOTS ##
###########
par(mfrow = c(2,1),mar=c(5.1, 4.1, 4.1, 2.1))
Draw_plot(dataRain,1,2,"", "Rainfall rate [mm/h]")
Draw_plot(dataAttenuation_diff,1,2,"Time [min]", "Signal [dB]")
abline(h =mean(dataAttenuation_diff[,2], untf = FALSE),lty=5,col=35)


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
FILE_path <- file.choose()
DIR_path <- dirname(FILE_path)
setwd(DIR_path)
source("Preprocessing.R")


myMicrowave <- Import_Data()
dataAttenuation <-Import_Data()
dataRain <- Import_Data()

aaaa <- dataAttenuation
dataAttenuation <- aaaa

dataAttenuation$date = strptime(dataAttenuation$date,"%Y-%m-%d %H:%M:%S")
dataRain$date = strptime(dataRain$date,"%Y-%m-%d %H:%M:%S")

dataAttenuation <- Check_NA(dataAttenuation)
dataRain <- Check_NA(dataRain)

dataAttenuation <- Average_Dates(dataAttenuation,"mins",1)
dataRain <- Average_Dates(dataRain,"mins",1)

dataAttenuation_aggregate <- Aggregate_data(dataAttenuation,10,"%Y-%m-%d %H:%M")
dataAttenuation_aggregate <- Complete_data(dataAttenuation_aggregate,2)
dataRain_aggregate <- Aggregate_data(dataRain,10,"%Y-%m-%d %H:%M")
dataRain_aggregate <- Complete_data(dataRain_aggregate,2)


par(mfrow = c(2, 2))
dataAttenuation_diff <- Time_diff(dataAttenuation,"mins")
Draw_plot(dataAttenuation_diff,1,2,"Time [min]", "Attenuation [dB]")

dataRain_diff <- Time_diff(dataRain,"mins")
Draw_plot(dataRain_diff,1,2,"Time [min]", "Rainfall rate [mm/h]")


par(mfrow = c(2, 2))
plot(dataAttenuation_aggregate[,1],dataAttenuation_aggregate[,2],type="n")
points(dataAttenuation_aggregate[,1],dataAttenuation_aggregate[,2],type="l",pch = ".")

plot(dataAttenuation_diff[,1],dataAttenuation_diff[,2],type="n")
points(dataAttenuation_diff[,1],dataAttenuation_diff[,2],type="l",pch = ".")

plot(dataRain_diff[,1],dataRain_diff[,2],type="n")
points(dataRain_diff[,1],dataRain_diff[,2],type="l",pch = ".")

plot(dataRain_aggregate[,1],dataRain_aggregate[,2],type="n")
points(dataRain_aggregate[,1],dataRain_aggregate[,2],type="l",pch = ".")

data_Rain_fft <- fft(as.numeric(dataRain_diff[,2]), inverse = FALSE)

plot(fft(as.numeric(dataRain_diff[,2]), inverse = FALSE), type = 'p',pch="x")

plot(data_Rain_fft[abs(data_Rain_fft) > 20],type = 'p', pch="x")

high_freq <- data_Rain_fft[abs(data_Rain_fft) > 20]

inv_high_freq <- fft(high_freq, inverse = TRUE)/length(high_freq)

plot(inv_high_freq, type = 'p', pch="x")

# test <- Aggregate_data(dataAttenuation,1,"%Y-%m-%d %H:%M")
# test_completed <- Complete_data(test,2)
# Draw_plot(test_completed,1,2,"Time [min]", "Attenuation [dB]")



# x <- 1:100
# y1 <- rnorm(100)
# y2 <- rnorm(100)+100
# 
# par(mar=c(5,5,5,5))
# 
# plot(x,y1,pch=".",type="l",col="red",yaxt="n",ylim=c(-8,2),ylab="")
# axis(side=2, at=c(-2,0,2))
# mtext("red line", side = 2, line=2.5, at=0)
# 
# par(new=TRUE)
# plot(x,y2,pch=1,type="b",col="blue",yaxt="n",ylim=c(98,108), ylab="")
# axis(side=4, at=c(98,100,102), labels=c("98%","100%","102%"))
# mtext("blue line", side=4, line=2.5, at=100)

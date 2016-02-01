## =======================================================
## Project:
##
## Description: change date format
##
## File: convert_time.r
## Path: c:/Users/scheidan/Dropbox/Eawag/Rainfall assimilation/Adliswil/Misc/Gauges/
##
## January 13, 2014 -- Andreas Scheidegger
##
## andreas.scheidegger@eawag.ch
## =======================================================


setwd("c:/Users/vrzbamir/CAIRS/data/Event_4/MWL/raw/")

files <- dir(pattern="txt")


for(file in files) {
  
  data <- read.table(file, sep=" ", colClasses=c("character", "character", "character", "numeric"))
  
  if(sum(data[,3]!="")>0) stop("Values in columns 3!")
  data <- na.omit(data[,c(1, 2, 4)])
  
  time.str <- paste(data[,1], data[,2])
  time <- strptime(time.str, "%Y-%m-%d %H:%M:%S")
  
  ## convert in UTC (i.e. 2 hours difference for summer, 1 hour difference for winter)
  time <- time - 2*3600
  time <- format(time, "%d.%m.%Y %H:%M:%S")
  
  ## Power-Law, values from: ITU Radiocommunication Assembly, ITU-R P.838-3
  ## R = (A/(length*k))^(1/alpha), A[dB/km], R[mm/h]
  ## Additionally a 3dB offset is applied due to antenna wetting (Joerg Rieckermann)
  
  alpha.23GHz.H <- 1.0214
  k.23GHz.H <- 0.1286
  
  alpha.23GHz.V <- 0.9630
  k.23GHz.V <- 0.1284
  
  alpha.38GHz.H <- 0.8816
  k.38GHz.H <- 0.4001
  
  alpha.38GHz.V <- 0.8552
  k.38GHz.V <- 0.3844
  
  alpha.58GHz.H <- 0.7731
  k.58GHz.H <- 0.8226
  
  alpha.58GHz.V <- 0.7552
  k.58GHz.V <- 0.8129
  
  
  if(grepl("ZH9809A_ZH7865A", file)) {length <- 3016; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H
  if(grepl("ZH9809A_ZH7844A", file)) {length <- 2174; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H
  if(grepl("ZH9809A_ZH0615B", file)) {length <- 6316; k <- k.23GHz.H; alpha <- alpha.23GHz.H} #23GHz, H
  #if(grepl("ZH0235C_ZH0217A", file)) {length <- 2290; k <- k.38GHz.V; alpha <- alpha.38GHz.V} #38GHz, V
  #if(grepl("ZH0218A_ZH0288A", file)) {length <- 2018; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H
  #if(grepl("ZH0218A_ZH7866A", file)) {length <- 2138; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H
  
  #if(grepl("ZH0233C_ZH0675H", file)) {length <- 1775; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H        no data !?
  #if(grepl("ZH0694A_ZH0835A", file)) {length <- 2482; k <- k.38GHz.V; alpha <- alpha.38GHz.V} #38GHz, H
  #if(grepl("ZH0694A_ZH8020A", file)) {length <- 584;  k <- k.58GHz.V; alpha <- alpha.58GHz.V} #58GHz, V
  #if(grepl("ZH7527A_ZH0027C", file)) {length <- 294;  k <- k.58GHz.V; alpha <- alpha.58GHz.V} #58GHz, V
  #if(grepl("ZH9802A_ZH4010C", file)) {length <- 1410; k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H        no data !?
  #if(grepl("ZH9908B_ZH0580B", file)) {length <- 914;  k <- k.38GHz.H; alpha <- alpha.38GHz.H} #38GHz, H        no data !?
  
  
  data.atten <-  pmax(0, -1*(data[,3]-(median(data[,3]-3))))
  data.rain <- (data.atten*1000/(length*k))^(1/alpha)
  
  data <- data.frame(time, data.rain)
  
  print(paste(file, "- largest rain [mm/h]:", round(max(data[,2]), 3)))
  write.table(data,paste0("MWL_", file), sep=",", quote=FALSE, col.names=FALSE, row.names=FALSE) 
}

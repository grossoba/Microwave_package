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



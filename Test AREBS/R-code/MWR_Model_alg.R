# TODO: Add comment
# 
# Author: scaz
###############################################################################


library("MASS")
library("date")
library("chron")
library(TTR)
library(xts)
library(lattice)
library(IDPmisc)


# Paths
mw.wd <- "/home/dwhtest/Microwave_package/Test AREBS/R-code/"
mw.data.dir <- "/home/dwhtest/Microwave_package/Test AREBS/r-code/Data/"

# Working directory
setwd(mw.wd)

# loading Data files
# if these files are loaded, the data preparation part can be left out!!!
load("rainWork.rda")
load("mwWork.rda")
load("mwr.rda")
load("adWork.rda")
source("rfAsymBaseline.R")





################################################################################
################################################################################
################################################################################
################################################################################
#
# Data preparation
#
################################################################################
################################################################################
################################################################################
################################################################################


#-----------------------------------------------
# Load data files
#-----------------------------------------------

# microwave data
mw.main = read.table(paste(mw.data.dir,"/rain_links.txt",sep=""),header=T)
# rain data
rain.main = read.table(paste(mw.data.dir,"/rain_distrometer_EPFL.txt",sep=""),header=F)
# additional data (not needed)
#ad.main = read.table(paste(mw.data.dir,"/ad_w.txt",sep=""),header=T, sep=",")



#-----------------------------------------------
# Data editing
#-----------------------------------------------

# All the Data will be reduced to a one minute base

# mw Data
#--------
# the mw Data usually contains 15 measuring points in one minute.
# the mw Strength will be averaged on a minutes base.

# create one date variable from time and date
mw.main$date = paste(mw.main$data,mw.main$time)
mw.main$date = strptime(mw.main$date,"%m/%d/%y %H:%M:%S") # generating POSIXlt for date and time

# select important data rows for new data set
mw.b1 = mw.main[,c(5,3,4)]


# Save the original data
mw.orig = mw.b1
if(!file.exists("mwOrig.rda")){
	save(mw.orig, file="mwOrig.rda")
}


#------------------------------
# Aggregate MWL data on a minutes base
#------------------------------
p.i = endpoints(mw.b1$date,on="mins")
sig.av =0
sig.r=0
for(i in 1:(length(p.i)-1)){
	sig.av[i] = mean(mw.b1$signal[(p.i[i]+1):p.i[i+1]]) 
	sig.r[i] = diff(range(mw.b1$signal[(p.i[i]+1):p.i[i+1]]))
#	sig.r[i] = diff(quantile(mw.b1$signal[(p.i[i]+1):p.i[i+1]],c(0.05,0.95)))	#c(0.1,0.9) kaum ein unterschied zu c(0.05,0.95)
}

mw.b2 = mw.b1[p.i,]
mw.b2$sig.av = sig.av
mw.b2$sig.r = sig.r

mw.work = mw.b2[,c(1,4,5,3)]

if(!file.exists("mwWork.rda"))
{  
	save(mw.work, file="mwWork.rda")	
}

# rain Data
#-----------
# the mw Data usually contains 2 measuring points in one minute.
# The rain is agrigatet do one minte time stepps. To do that the rain in of
# the tho measurings during the minute is averaged. 


# to deal with the missing measurements, the fallowing algorithm is used:
# if one measurement ist missing, there will be inserted the average of the
# missings right and left neighbors.
# two measurements are missing they will be handled the same way and get the 
# same value.
# if more than two measuring points are missing the measurement will be left on
# NA
# when the summs for one mitue rain is built there is no NA allowed. if there
# is one NA in the sum, the Sum will be NA.


rain.b0 = rain.main

#-------------------------------
# transform date format
#-------------------------------
rain.b0$date = paste(rain.b0[,1],rain.b0[,2])
rain.b0$date = strptime(rain.b0$date,"%Y-%m-%d %H:%M:%S") # generating POSIXlt for date and time

# set up data frame
rain.b1 = rain.b0[,c(4,3)]
colnames(rain.b1) = c("date","rain")

# save the original data
rain.orig = rain.b1
if(!file.exists("rainOrig.rda")){
	save(rain.orig, file="rainOrig.rda")
}



#-------------------------------
# remuving the NA's in the above mentioned way
#---------------------------------------
na.ind = which(is.na(rain.b1$rain))

for(i in na.ind){
	if(is.na(rain.b1$rain[i])){
		if(!is.na(rain.b1$rain[i-1])){
			if(!is.na(rain.b1$rain[i+1])){
				rain.b1$rain[i] = mean(rain.b1$rain[i+1],rain.b1$rain[i-1])
			}
			if(is.na(rain.b1$rain[i+1]) & !is.na(rain.b1$rain[i+2])){
				rain.b1$rain[i] = mean(rain.b1$rain[i+2],rain.b1$rain[i-1])
				rain.b1$rain[i+1]=rain.b1$rain[i]
			}
		}
	}
}


#-------------------------------
# aggregate rain on mitute basis
#-------------------------------
p.2 = endpoints(rain.b1$date,on="mins")
rain.av = 0
for(i in 1:(length(p.2)-1)){
	if(!is.na(rain.b1$rain[p.2[i+1]]) & !is.na(rain.b1$rain[p.2[i]+1])){
		rain.av[i] = mean(rain.b1$rain[p.2[i+1]],rain.b1$rain[p.2[i]+1])
	}else{
		rain.av[i] = NA
	}
}

rain.b2 = rain.b1[p.2+1,]
rain.b2 = rain.b2[-1,]
rain.b2$rain.av = rain.av

rain.work = rain.b2

# save rain data
if(!file.exists("rainWork.rda"))
{  
	save(rain.work, file="rainWork.rda")	
}
#load("rainWork.rda")




#-------------------------------
# Create working data frame
#-------------------------------

# inverting the mwr-signal
mw.work$sig.inv = mw.work$sig.av * (-1) 
# count (minutes) vector for mwr-signal
mw.work$x = as.numeric(mw.work$date)
mw.work$x = round((mw.work$x-mw.work$x[1])/60,0)

# Build Wroking dataset
mwr2 = cbind(mw.work[,c(1,6,5)],rain.work$rain.av,rep(NA,dim(mw.work)[1]),rep(NA,dim(mw.work)[1]))
colnames(mwr2) = c("date","x","sig.inv","rain.av","qr","qrm")

# calculating quantile difference in a period of 8
mwr = mwr2
per = 8
mwr$qr = 0
mwr$qrm =0

for(i in per:length(mwr$sig.inv)){
	mwr$qr[i]=as.numeric(diff(quantile(mwr$sig.inv[(i-per):i],c(0.1,0.9))))
}

# moving average
mwr$qrm=EMA(mwr$qr,n=20,wilder=FALSE)


# working data frame "mwr"
mwr = mwr2
if(!file.exists("mwr")){
	save(mwr, file="mwr.rda")
}






################################################################################
################################################################################
################################################################################
################################################################################
#
# Modeling
#
################################################################################
################################################################################
################################################################################
################################################################################





###############################################################################
# baseline extraction
################################################################################

#analyzing variability in time seiries with mad
mad.mwr = mad(mwr$qr)
var.l = 3*mad.mwr

# remove values over var.l for rfbaseline
i.o = which(mwr$qrm > var.l)
mwr.red = mwr[-i.o,]

# baseline extraction
NoXP.l=50				# number of used points
mwr.rfb = rfbaseline(x=mwr.red$x,y=mwr.red$sig.inv,NoXP=NoXP.l,
		delta=1/length(mwr$x),Scale = function(r) mad(r))


#linear approximation for removed values of time series
app.out = approx(x=mwr.red$x,y=mwr.rfb$fit,xout=mwr$x,method="linear")
mwr$fit = app.out$y

#subtract baseline from signal
mwr$sig.sub = mwr$sig.inv-mwr$fit



# plotting
#-----------------------
#plot(mwr$sig.sub,mwr$rain.av,type="n",ylab=expression("Rainfall Rate [mm/h]"),
#		xlab=expression("Attenuation - Baseline [dB]"),las=1)
#points(mwr$sig.sub,mwr$rain.av,cex=0.4,pch=20)



## plotting baseline and rainy periods
#mwr$r.col = mwr$sig.inv
#mwr$r.col[which(mwr$rain.av == 0)] = NA
#
#plot(mwr$x[69000:73000],mwr$sig.inv[69000:73000],cex=0.3,type="l",
#		main=paste("NoXP = ", NoXP.l,sep=" "),xlab="Time [Min]", ylab="A")
#points(mwr$x[69000:73000],mwr$fit[69000:73000],col="red",type="l")
#points(mwr$x[69000:73000],mwr$r.col[69000:73000],col="blue",type="l")




###################################
# Linear Models
###################################

# remove non-rain events
#		signal smaller than 2.5dB
#		rain less than 2mm/h
i1 = which(mwr$sig.sub > 2.5)
mwr2.1 = mwr[i1,]
i2 = which(mwr2.1$rain.av > 0.2)
mwr2 = mwr2.1[i2,]

# remove outliers for better estimation
plot(mwr$sig.sub,mwr$rain.av)
#identify(mwr2$sig.sub,mwr2$rain.av)
ausreisser = c(208,558,562,756)
mwr2 = mwr2[-ausreisser,]


# transform values for better estimation (log transformation)
mwr2$trans.y = log(mwr2$rain.av)
mwr2$trans.x = log(mwr2$sig.sub)

# remove outliers for better estimation
#plot(mwr2$trans.x,mwr2$trans.y)
#identify(mwr2$trans.x,mwr2$trans.y)
ausreisser2 = c(8,69,70,71,72,89,90,138,139,140,189,211,230,271,275,569,570,573)
mwr3 = mwr2[-ausreisser2,]


# Regression Model
mwr.fitlog = lm(trans.y ~ trans.x, data = mwr3) 
summary(mwr.fitlog)

if(!file.exists("mwrfitlog.rda")){save(mwr.fitlog, file="mwrfitlog.rda")}

#plotting
plot(x=seq(0,5,length=length(mwr3$trans.x)),y=seq(0,5,length=length(mwr3$trans.y)),type="n",
		main="Regression log(rain.av) ~ log(sig.sub)",xlab="log(sig-bl)",ylab="log(rain)")
points(x=mwr3$trans.x, y=mwr3$trans.y)
abline(mwr.fitlog,col="red")

# residual analysis
par(mfrow=c(2,2))
plot(mwr.fitlog)
mtext("Regression Residuals with NoXP=50",side=3,line=23.5,adj=5,cex = 1.1)

# plotting predicted values
x1 = seq(0,30,length=length(mwr3$trans.x))
y.p = predict(mwr.fitlog, newdata = data.frame(trans.x = mwr3$trans.x))
plot(x=exp(y.p+0.35^2/2),y=mwr3$rain.av,xlab="Predicted values",ylab="real values",
		main="Rain vs. predicted rain")
#lines(x1,exp(x.p),col="red")
abline(a=0,b=1, col="red")




# weitere Plotts
plot(x=seq(1,length(y.p),1),y=seq(min(exp(y.p+0.35^2/2)),max(exp(y.p+0.35^2/2)),length=length(y.p)),type="n")
lines(x=seq(1,length(y.p),1),exp(y.p+0.35^2/2),col="red")
lines(x=seq(1,length(y.p),1),mwr3$rain.av,col="blue")
y.p.sub = mwr3$rain.av-exp(y.p+0.35^2/2)
plot(x=seq(1,length(y.p),1),y.p.sub,col="green")
sum(y.p.sub)


# plotting aggregation
n=1
y.s = filter(mwr3$rain.av,rep(1,n)/n,side=1)
y.ps = filter(y.p,rep(1,n)/n,side=1)

v.temp=0
y.var.mat = matrix(NA,nrow=length(y.p),ncol=n)

for(i in 1:n){
	if(i==1){v.temp = y.p}
	if(i>1){v.temp = c(y.p[i:length(y.p)],rep(NA,(i-1)))}
	y.var.mat[,i] = v.temp
}
var.mat = apply(y.var.mat,1,function(x) var(x))







###############################################################################
###
###			Asymetric case
###
###############################################################################


load("mwrfitlog.rda")

N = 70000				# how many loops should be done (how many values guessed)
hist = 10000
per = 8

mwr.asym = mwr[,c(1,3)]
mwr.asym$sig = mwr.asym$sig.inv *(-1)
mwr.asym$rain.av = mwr$rain.av
mwr.asym = mwr.asym[,-2]
mwr.asym$sig.inv = 0
mwr.asym$qr = 0
mwr.asym$qrm = 0
mwr.asym$bl = 0
mwr.asym$sig.sub = 0
mwr.asym$pred.rain = 0
mwr.asym$sig.inv[1:hist] = mwr$sig.inv[1:hist]
mwr.asym$qr[1:hist] = mwr$qr[1:hist]
mwr.asym$qrm[1:hist] = mwr$qrm[1:hist]


qrm.temp = 0
sig.bl = 0
asym.sig.inv = mwr.asym


fun.asym = function(i){
	
	mwr.asym$sig.inv[hist+i] <<- mwr.asym$sig[hist+i]*(-1)	# inverting signal
	
	#quantile difference for estimating mad
	mwr.asym$qr[hist+i] <<- as.numeric(diff(quantile(mwr$sig.inv[((hist+i)-per):(hist+i)],c(0.1,0.9))))
	
	#exponentially filter on quantile difference
	qrm.temp = EMA(mwr.asym$qr[1:(hist+i)],n=20,wilder=FALSE)  # muss ?berarbeitet werden f?r Real-Einsatz
	mwr.asym$qrm[hist+i] <<- qrm.temp[length(qrm.temp)]			
	
	#estimating mad over quantile difference
	mad.asym = mad(mwr.asym$qr[(hist-10000+i):(hist+i)])
	var.asym = 3*mad.asym
	
	# removing baseline linear case
	if(mwr.asym$qrm[hist+i] < var.asym){
#		print(mwr.asym$qrm[hist+i])
#		print(var.asym)
		NoXP.asym = 50				# number of used points
		mwr.asym.rfb = rfAsymBaseline(x=mwr.asym$date[(hist-40+i):(hist+i)],
				y=mwr.asym$sig.inv[(hist-40+i):(hist+i)], 
				NoXP=ceiling(NoXP.asym/2),
				delta=1/length(hist+i), Scale = function(r) mad(r))
		mwr.asym$bl[hist+i] <<- mwr.asym.rfb$fit[length(mwr.asym.rfb$fit)]
	}else{
		if(mwr.asym$bl[hist+i-1] < 59){
			mwr.asym$bl[hist+i] <<- mwr.asym$bl[hist+i-1]+0.005
		}else{
			if(mwr.asym$bl[hist+i-1] > 59.1){
				mwr.asym$bl[hist+i] <<- mwr.asym$bl[hist+i-1]
			}else{
				mwr.asym$bl[hist+i] <<- 59	
			}
		}
	}
	
	mwr.asym$sig.sub[hist+i] <<- mwr.asym$sig.inv[hist+i]-mwr.asym$bl[hist+i]
	
	if(mwr.asym$sig.sub[hist+i] > 0){
		trans.xx = log(mwr.asym$sig.sub[hist+i])
		mwr.asym$pred.rain[hist+i] <<- predict(mwr.fitlog,newdata = data.frame(trans.x = trans.xx))
		mwr.asym$pred.rain[hist+i] <<- exp(mwr.asym$pred.rain[hist+i])
	}else{
		mwr.asym$pred.rain[hist+i] <<- 0
	}
}

pred.vec  = sapply(X=1:N,FUN=fun.asym)
prt2[3]-prt1[3]
mwr.asym$fit = mwr.asym$sig.inv-mwr.asym$sig.sub


mwr.asym$pred.rain = log(mwr.asym$pred.rain)
mwr.asym$pred.rain = exp(mwr.asym$pred.rain+(0.36^2/2))

save(mwr.asym,file = "mwr.asym.V5.rda")
#load("mwr.asym.V5.rda")





##########################################################
#  Plotting examples
##########################################################


# mwr.colvar = mwr.asym$sig.inv[37028:39028]
# mwr.colvar[which(mwr.asym$)]

plot(mwr.asym2$sig.inv,col="red",type="l")
lines(mwr.asym2$fit,col="blue")



plot(mwr.asym$sig.inv[55000:58000],col="red",type="l")
lines(mwr.asym$fit[55000:58000],col="blue")





plot(mwr.asym$pred.rain[30000:87800],mwr.asym$rain.av[30000:87800])
identify(mwr.asym$pred.rain[30000:87800],mwr.asym$rain.av[30000:87800])
abline(a=0,b=1,col="red")
fit2 = lm(rain.av~pred.rain,data=mwr.asym[30000:87800,])
summary(fit2)

mwr.asym2 = mwr.asym[30000:87800,]
mwr.asym2 = mwr.asym2[-c(1039,40329,40333,56304),]

plot(mwr.asym2$pred.rain,mwr.asym2$rain.av)
identify(mwr.asym2$pred.rain,mwr.asym2$rain.av)
abline(a=0,b=1,col="red")
abline(fit3)
fit3 = lm(rain.av~pred.rain,data=mwr.asym2)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)


# plotting aggregation
y.p = mwr.asym$pred.rain[30000:87000]
n = 1
y.s = filter(mwr.asym$rain.av[30000:87000],rep(1,n)/n,side=1)
y.ps = filter(y.p,rep(1,n)/n,side=1)

plot(x=y.ps,y=y.s,xlab="Predicted values",ylab="real values",
		main="agregated rain vs. predicted rain")
abline(a=0,b=1, col="red")

fit.test= lm(y.s~y.ps)
abline(fit.test)















#delete old data
rm(list=ls())

#datetime
#POSIXCT signed number of seconds since "01/01/1970 at 00:00:00 UTC without leap seconds
#POSIXLT one of many text|character|string format such os 17-05-14 09:98:67

#define a root folder where the tree of folders os saved
rootfolder <- "D:\\RainfallSpRecon"

###############################
## load libraries, source functions and data
###############################
setwd(paste(rootfolder, "\\DataBase", sep=""))


library(RPostgreSQL)
#library(xts)
library("date")
library("chron")

#you have two type : ODBC & DBI 
# we use DBI for PostgreSQL

#connect to DB PostGres you need to install the RPostgreSQL package 

# name of database
db <- "rain"

#timestamp(minutes)
loop <- 1
#NODE ID
node <- 8


# load the pilot

drv <- dbDriver("PostgreSQL")

#connect to DB using driver

con <- dbConnect(drv, dbname= db, user = "postgres" ,password="postgres")



####plot RSL during the time####

# create request
node<-as.character(node)
sql0 <- 'SELECT created_at, rx_power FROM measurements where node_id='
sql <- paste(sql0,' order by created_at asc  ;',sep = node)
# run the command

res <- dbGetQuery(conn = con, statement = sql)

# Data editing
#-----------------------------------------------

# All the Data will be reduced to a one minute base

# mw Data
#--------
# the mw Data usually contains meaning measuring points (02) in one minute.
# the mw Strength will be averaged on a minutes base.

# to deal with the missing measurements, the following algorithm is used:
# if one measurement ist missing(no value , no time), there will be inserted 
#the average of the missings right and left neighbors.
# two measurements are missing they will be handled the same way and get the 
# same value.

# create one date variable from time and date
res$created_at = as.POSIXct(strptime(res$created_at,"%Y-%m-%d %H:%M")) # generating POSIXct for date and time

#------------------------------
# Aggregate MWL data on a minutes base
#------------------------------

#size of rsl
dim <- range(res$created_at)
#data.frame for preprocessing

mwWork <- seq( dim[1], dim[2],loop*60)

#convert raw datatimes to minutes with the reference the first datetime
temp <- as.integer((res$created_at- res$created_at[1])/60)

#convert final datatime to minutes with the reference the first datetime
mwWork <-as.integer((mwWork-mwWork[1])/60)




sig.av =0
sig.r=0

for( k in 1:length(mwWork))
{
  if((mwWork[k] %in% temp) == TRUE)
  {
    
    sig.av[k]= mean(res$rx_power[which(temp==mwWork[k])])
    sig.r[k]=diff(range(res$rx_power[which(temp==mwWork[k])]))
    
  }
  else
  {
    sig.av[k]=sig.av[k-1]
    sig.r[k]=0
  }
    
}
  


#convert to data.frame
mwWork <-data.frame(datetime= mwWork)
mwWork$sig.av = sig.av
mwWork$sig.r = sig.r


#save to file RDATA
save(mwWork, file="mwWork.RData")	



## plotting attenuation(RSL) periods

# plot(mwWork$created_at,mwWork$sig.av,type="n", ylab="A[db]", xlab = "Time[mn]")
# points(mwWork$created_at,mwWork$sig.av,col="red", pch="*")

plot(mwWork$sig.av,type="n", ylab="A[db]", xlab = "Time")
points(mwWork$sig.av,col="red", pch="*", type = "p")

# close connexion
dbDisconnect(con)
#unload pilote
dbUnloadDriver(drv)


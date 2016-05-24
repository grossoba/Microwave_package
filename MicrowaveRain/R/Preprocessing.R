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

#############################
## IMPORT DATA FROM A FILE ##
#############################
Import_Data <- function(){
  print("Choose your data file")
  file_path <- file.choose()
  mydata = read.table(file_path,header = TRUE,sep = ";")
  return(mydata)
}
Import_Data <- function(file_path, ...){
  mydata = read.table(file_path,header = TRUE,sep = ";", stringsAsFactors=FALSE, ...)
  return(mydata)
}


##############################################
## MAKE THE AVERAGE FOR GIVEN TIME INTERVAL ##
##############################################
# This functions takes as input a table with in the first column the date and in the second one
# the intensity. First it splits the lines between intervals given by "the time_unit" and "dt" input
# variables. It returns a table with average intensity and last time value of the interval
###############################################
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

###########################
## CHECK IF NOT A NUMBER ##
###########################
# The function detects if an entry is not a number and in this case it makes the average with the 
# previous and next neighbour
###############################################
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

#########################################################################
## CONVERTS DATES INTO TIME DIFFERENCIES STARTING FROM THE FIRST VALUE ##
#########################################################################
Time_diff <- function(Intensity,time_unit)
{
  Intensity_diff <- matrix(0,nrow = (length(Intensity[,1])), ncol = 2)

  for(i in 1:length(Intensity[,1]))
  {
  Intensity_diff[i,1] <- as.numeric(difftime(Intensity[i,1],Intensity[1,1], units=c(time_unit)))
  Intensity_diff[i,2] <- Intensity[i,2]

  }
  return(Intensity_diff)
}

###############################################
## IMPORT DATA FROM A DATABASE INTO A MATRIX ##
###############################################
# function to get data (time and attenuation for one link during one ordered period range)
# return a data.frame which contains datetime and rx_power level 

# don't forget to make one request ordered 
# ensure that the PostgreSQL DB is available and you have the good account to this server

# you have two types : ODBC & DBI 
# we use DBI for PostgreSQL

#variables
# db is the database name
# usr is the username on DB
# pasw is the password on DB
# requestSQL is the SQL request to get the ordered data (low ->high datetime only)
# to connect to DB PostGres you need to install the RPostgreSQL package and load the pilot
###############################################
Getpg_SQL <- function(db,usr,pasw,requestSQL)
{                                                                        
  drv <- dbDriver("PostgreSQL")
  
  #connect to DB using driver
  con <- dbConnect(drv, dbname= db, user = usr ,password=pasw)

  # create request and run the command
  res <- dbGetQuery(conn = con, statement = requestSQL)

  # close connexion
  dbDisconnect(con)
  
  #unload pilote
  dbUnloadDriver(drv)
  return(res)
}

###################################################
## GATHER THE DATA ON A GIVEN BASE (EVERY X-MIN) ##
###################################################
# function to aggregate data in the specific period and complete when you 
# don't have the data using mean function and save the result

#variables
# res is the data.frame which contains the datetime and attenuation
# typeformat is the format of datetime without the second eg "%Y-%m-%d %H:%M" for 2014-12-11 12:21:23
# loop is the aggregate time in minutes only

# to deal with the missing measurements, the following algorithm is used:
# if one measurement is missing (no value , no time), there will be inserted 
# the average of the missings right and left neighbors.
# two measurements are missing they will be handled the same way and get the 
# same value.
###############################################
Aggregate_data <-function(res,loop,typeformat)
{
  # create one date variable from time and date
  res[,1] = as.POSIXct(strptime(res[,1],typeformat)) # generating POSIXct for date and time
  
  #size of rsl
  dim <- as.numeric(range(res[,1]))

    #data.frame for preprocessing
  mwWork <- seq( dim[1], dim[2],loop*60)

  #convert raw datatimes to minutes with the reference the first datetime
  temp <- (as.integer(res[,1])- as.integer(res[1,1]))/60

  #convert final datatime to minutes with the reference the first datetime
  mwWork <-(as.integer(mwWork)-as.integer(mwWork[1]))/60
 
  
  sig.av =0
  sig.r=0
  
  for( k in 1:length(mwWork))
  {
    if((mwWork[k] %in% temp) == TRUE)
    {
      sig.av[k]= mean(as.numeric(res[which(temp==mwWork[k]),2]))
      sig.r[k]=diff(range(as.numeric(res[which(temp==mwWork[k]),2])))
    }
    else
    {
      sig.av[k] = 999999
      sig.r[k]=0
    }
  }
  
  #convert to data.frame
  mwWork <-data.frame(datetime= mwWork)
  mwWork$sig.av = sig.av
  mwWork$sig.r = sig.r
  
  #save to file RDATA
  save(mwWork, file="mwWork.RData")	
  
  return(mwWork)
}

######################################################
## COMPLETE THE EMPTY VALUES TO HAVE A REGULAR TIME ##
######################################################
Complete_data <- function(file,field)
{
  #complete the missing values like 999999 
  tmp_before <-0
  tmp_after <- 0
  for (i in 1:length(file[,field]))
  {
    if (file[i,field]!=999999)
    {
      tmp_before <- as.numeric(file[i,field])
    }
    else
    {
      incr <- i
      while(file[incr,field]==999999 && incr < length(file[,field]))
      {
        incr <- incr + 1
      }
      
      tmp_after <- as.numeric(file[incr,field])
      
      for(j in i:(incr-1))
      {
        file[j,field] <- (tmp_before+tmp_after)/2
      }
    }
  }
  return(file)
}

###############
## PLOT DATA ##
###############
Draw_plot <- function(data,x,y,xlabel,ylabel,...)
{
  plot(strptime(data[,x],"%Y-%m-%d %H:%M:%S"),data[,y],type="l",pch=".",ylim=(as.numeric(c(min(data[,y]),max(data[,y])))),xlab=xlabel, ylab=ylabel,...)
  # points(strptime(data[,x],"%Y-%m-%d %H:%M:%S"),data[,y],type="l",pch = ".", ...)
  
}











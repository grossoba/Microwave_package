<<<<<<< HEAD
#funtions: for data preporocessing (by Bastien and William)
## getpgsql - get result of your request from PosTGREsqlDB 
## aggregatedata - aggregate the data in correct form

getpgsql <- function(db,usr,pasw,requestSQL)
  {                                                                        
  
  # function to get data (time and attenuation for one link during one period range in order)
  # return a data.frame which contains datetime and rx_power level 
  
  #define theses libraries in the top of your main file before used it
  #library(RPostgreSQL)
  
  # don't forget to make one request ordered 
  # ensure that the PostgreSQL DB is available and you have the good account to this server

  #you have two type : ODBC & DBI 
  # we use DBI for PostgreSQL
  
  #variables
  #db is the database name
  #usr is the name of user of DB
  #pasw is the name of password DB
  #requestSQL is the SQL request to get the data in order (low ->high datetime only)
  
  
  #connect to DB PostGres you need to install the RPostgreSQL package 
  
  # load the pilot
  
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

aggregatedata <-function(ress,loop,foolder,typeformat)
{
  # function to aggregate data in the specific period and complete when you 
  # don't have the data using mean function and save the result in the specific
  # folder gives by you
  
  #define theses libraries in the top of your main file before used it
  #library(xts)
  #library("date")
  #library("chron")
  
  #variables
  #res is the data.frame which contains the datetime and attenuation
  #typeformat is the format of datetime without the second eg "%Y-%m-%d %H:%M" for 2014-12-11 12:21:23
  #loop is the aggregate time in minutes only < 60 minutes
  #foolder is the path where you save the result 
 
  # Data editing
  #-----------------------------------------------
  
  # All the Data will be reduced to a one minute base
  
  # mw Data
  #--------
  # the mw Data usually contains meaning measuring points (02 or more) in one minute.
  # the mw Strength will be averaged on a minutes base.
  
  # to deal with the missing measurements, the following algorithm is used:
  # if one measurement ist missing(no value , no time), there will be inserted 
  #the average of the missings right and left neighbors.
  # two measurements are missing they will be handled the same way and get the 
  # same value.
  
  
  #datetime
  #POSIXCT signed number of seconds since "01/01/1970 at 00:00:00 UTC without leap seconds
  #POSIXLT one of many text|character|string format such os 17-05-14 09:98:67
  
  
  # create one date variable from time and date
  ress[,1] = as.POSIXct(strptime(ress[,1],typeformat)) # generating POSIXct for date and time
  
  #------------------------------
  # Aggregate MWL data on a minutes base
  #------------------------------
  
  #size of rsl
  dim <- range(ress[,1])
  #data.frame for preprocessing
  
  mwWork <- seq( dim[1], dim[2],loop*60)
  
  #convert raw datatimes to minutes with the reference the first datetime
  temp <- as.integer((ress[,1]- ress[1,1])/60)
  
  #convert final datatime to minutes with the reference the first datetime
  mwWork <-as.integer((mwWork-mwWork[1])/60)
  
  
  
  
  sig.av =0
  sig.r=0
  
  for( k in 1:length(mwWork))
  {
    if((mwWork[k] %in% temp) == TRUE)
    {
      
      sig.av[k]= mean(ress[which(temp==mwWork[k]),2])
      sig.r[k]=diff(range(ress[which(temp==mwWork[k]),2]))
      
    }
    else
    {
      sig.av[k]=0
      sig.r[k]=0
    }
    
    
  }
  
  
  #convert to data.frame
  mwWork <-data.frame(datetime= mwWork)
  mwWork$sig.av = sig.av
  mwWork$sig.r = sig.r
  
  
  ###############################
  ## load libraries, source functions and data
  ###############################
  setwd(folder)
  
  
  #save to file RDATA
  save(mwWork, file="mwWork.RData")	
  
  
  return(mwWork)
  
}


completvectdata <- function(vec)
{
  tmp_before <-0
  tmp_after <- 0
  for (i in 1:length(vec))
  {
    if (vec[i] != 0)
    {
      tmp_before <- as.numeric(vec[i])
    }
    else
    {
      incr <- i
      while(vec[incr]==0)
      {
        
        incr <- incr + 1
      }
      
      tmp_after <- as.numeric(vec[incr])
      
      
      for(j in i:(incr-1))
      {
        vec[j] <- (tmp_before+tmp_after)/2
        
      }
    }
  }
  return(vec)
}


completematdata <- function(vec,field)
{
  
  #complete the missing value like 0 
    
  tmp_before <-0
  tmp_after <- 0
  for (i in 1:length(vec[,field]))
  {
    if (vec[i,field] != 0)
    {
      tmp_before <- as.numeric(vec[i,field])
    }
    else
    {
      incr <- i
      while(vec[incr,field]==0)
      {
        
        incr <- incr + 1
      }
      
      tmp_after <- as.numeric(vec[incr,field])
      
      
      for(j in i:(incr-1))
      {
        vec[j,field] <- (tmp_before+tmp_after)/2
        
      }
    }
  }
  
  return(vec)
  
}
  
  

=======
#funtions: for data preporocessing (by Bastien and William)
## getpgsql - get result of your request from PosTGREsqlDB
## aggregatedata - aggregate the data in correct form

getpgsql <- function(db,usr,pasw,requestSQL)
  {

  # function to get data (time and attenuation for one link during one period range in order)
  # return a data.frame which contains datetime and rx_power level

  #define theses libraries in the top of your main file before used it
  library(RPostgreSQL)

  # don't forget to make one request ordered
  # ensure that the PostgreSQL DB is available and you have the good account to this server

  #you have two type : ODBC & DBI
  # we use DBI for PostgreSQL

  #variables
  #db is the database name
  #usr is the name of user of DB
  #pasw is the name of password DB
  #requestSQL is the SQL request to get the data in order (low ->high datetime only)


  #connect to DB PostGres you need to install the RPostgreSQL package

  # load the pilot

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

aggregatedata <-function(res,loop,foolder,typeformat)
{
  # function to aggregate data in the specific period and complete when you
  # don't have the data using mean function and save the result in the specific
  # folder gives by you

  #define theses libraries in the top of your main file before used it
  library(xts)
  library("date")
  library("chron")

  #variables
  #res is the data.frame which contains the datetime and attenuation
  #typeformat is the format of datetime without the second eg "%Y-%m-%d %H:%M" for 2014-12-11 12:21:23
  #loop is the aggregate time in minutes only
  #foolder is the path where you save the result

  # Data editing
  #-----------------------------------------------

  # All the Data will be reduced to a one minute base

  # mw Data
  #--------
  # the mw Data usually contains meaning measuring points (02 or more) in one minute.
  # the mw Strength will be averaged on a minutes base.

  # to deal with the missing measurements, the following algorithm is used:
  # if one measurement ist missing(no value , no time), there will be inserted
  #the average of the missings right and left neighbors.
  # two measurements are missing they will be handled the same way and get the
  # same value.

  # create one date variable from time and date
  res$created_at = as.POSIXct(strptime(res$created_at,typeformat)) # generating POSIXct for date and time

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
      sig.av[k]= mean(res[which(temp==mwWork[k]),2])
      sig.r[k]=diff(range(res[which(temp==mwWork[k]),2]))
    }
    else
    {
      sig.av[k]=999999
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


Complete_data <- function(file,field)

  {
  #complete the missing value like 999999

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
        while(file[incr,field]==999999)
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

>>>>>>> 313d75ce70d0e00361d86142877bd9bd86051fad

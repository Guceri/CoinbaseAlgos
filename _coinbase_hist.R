#GOTCHA'S: 
#This script will download full history but will only copy the last million lines of code
#CSV's have a limit to how much data they can hold so only the most recent million lines is included
#Daylight savings in Nov needs to be updated yearly
#If there is a new symbol, any empty data base must be created first then hardcode "mostRecent"
#If Coinbase is down, this will stay in loop 

#move column 'x' UP by 'n' spaces
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

pull_hist <- function(){
  setwd(mydatafolder)
  #store format of symbol for coinbase api request
  CandleRequestSymbol <<- pairdata
  #Convert format of pair for database
  pairdata <<-  paste0(substr(pairdata,5,7),"_",substr(pairdata,1,3))

  #============================================================================
  #####################################INPUTS##################################
  #============================================================================
  #current time rounded down to the minute and subtract 1 min
  currentTime <<- as.integer(floor_date(Sys.time(),unit = "minute"))-60#epoch
  #error handling inputs
  newWindow <<- 60
  errorMode <<- FALSE
  quotelimit <<- 300

  #============================================================================
  ###########################ANALYSE AVAILABLE DATA############################
  #============================================================================
  #Open connection to db file
  db <<- dbConnect(SQLite(),dbname="coinbase_0.1.db")
  #Pull data from db table
  mostRecent <<- dbGetQuery(db,paste0("select *, max(time) from candles_",pairdata))$time #epoch

  #default start times if database is empty
  if(is.na(mostRecent)){
    if(CandleRequestSymbol == "BTC-USD" || CandleRequestSymbol == "ETH-USD"){
      #1/1/17 @ 12:01AM EST
      mostRecent <<- 1483246860
    }else{
      #1/1/18 @ 12:01AM EST
      mostRecent <<- 1514782860  
    }
  }

{
# Debug dupes from data base. open DB browser, delete old db, and rename new one
# NOTE: don't forget to update column settings within the db
#db <- dbConnect(SQLite(),dbname="coinbase_0.1.db")
#df<- dbGetQuery(db,"select * from candles_USD_BTC")
#df <- distinct(df,time, .keep_all = T )
#dbWriteTable(db, "new_USD_BTC", df)
#dbDisconnect(db)
}

  #============================================================================
  #Total requests needed rounded up
  totalrequests <<- ceiling((currentTime-mostRecent)/60/quotelimit)
  # create progress bar
  pb <<- tkProgressBar(title = "Downloading Historical Data", min = 0, max = totalrequests, width = 300)
  #progress bar counter
  i <<- 0
  cat("Downloading Data....\n")
  
  #============================================================================
  ############################IDENTIFY DATA NEEDED#############################
  #============================================================================
  #Data request loop
  while (mostRecent < currentTime){
    #If there is no error create time window needed otherwise trycatch error set new windows
    if (!errorMode){
     #If the data needed is larger than or equal to max request size
     if((currentTime-mostRecent)/60>=quotelimit){
        cat("full batch request\n")
        start_time <<- strftime(as_datetime(mostRecent+60, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
        end_time <<- strftime(as_datetime(mostRecent+18060, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
      }else{ #If the data needed is smaller than max request
        cat("small batch request\n")
        start_time <<- strftime(as_datetime(mostRecent+60, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
        end_time <<- strftime(as_datetime(currentTime+60, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
      }
    }
    #reset errormode since start/end times have been updated to "new" window
    errorMode <<- FALSE
    candleData <<- tryCatch(public_candles(product_id = CandleRequestSymbol, start = start_time, end = end_time, granularity = 60),
                           error = function(e){
                              newWindow <<- newWindow + 18000
                              #create next chunk of data to request
                              start_time <<- strftime(as_datetime(mostRecent+newWindow, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
                              end_time <<- strftime(as_datetime(mostRecent+newWindow+18000, tz = "UTC"),"%Y-%m-%dT%H:%M:%S.000000Z", tz = "UTC")
                              Sys.sleep(1)
                              i <<- i +1
                              setTkProgressBar(pb, i, label=paste(round(i/totalrequests*100, 0),"% done"))
                              errorMode <<- TRUE
                              print(e)
                              cat("trying next window....\n")
                            })  
    #If there are no errors in data request
    if (!(errorMode)){
      cat("No errors, so updating db\n")
      #Convert time stamp into epoch format
      candleData$time <<- as.integer(candleData$time)
      #add new trading information to db table
      dbWriteTable(db, paste0("candles_",pairdata), candleData, append = TRUE)
      #search db for most recent time stamp
      mostRecent <<- dbGetQuery(db,paste0("select *, max(time) from candles_",pairdata))$time #epoch
      Sys.sleep(.75)
      #update progress bar
      i <<- i +1
      setTkProgressBar(pb, i, label=paste(round(i/totalrequests*100, 0),"% done"))
    }
  }

  close(pb)
  cat("Download Complete!!\n")

  #Close db
  dbDisconnect(db)
  #Give database a second to close on the computer
  Sys.sleep(2)
  
  #======================================================================================================================================
  #============================================================EXPORT TO CSV=============================================================
  #======================================================================================================================================
  cat("Transfering to csv...\n")
  
  #============================================================================
  ################################DAYLIGHT SAVINGS#############################
  #============================================================================
  ds2016<<-1478408400 #11/06/16 1AM timestamp
  ds2017<<-1509858000 #11/05/17 1AM timestamp
  ds2018<<-1541307600 #11/04/18 1AM timestamp
  ds2019<<-1572757200 #11/03/19 1AM timestamp
  dif<<-3600 #60 minutes converted to seconds
  
  #============================================================================
  ################################EXTRACT DATA#################################
  #============================================================================
  db <<- dbConnect(SQLite(),dbname="coinbase_0.1.db")
  data<-dbGetQuery(db,paste0("select * from candles_",pairdata))
  dbDisconnect(db)
  #sort the data by EPOCH time stamp (It might not be in chronoligical order)
  data<-data[order(data$time),]
  cat("Extracted data from Database....\n")
  cat("Formatting data for CSV....\n")
  
  #============================================================================
  ################################FORMAT DATE##################################
  #============================================================================
  #take out duplicate Daylight Savings TimeStamp for 2016
  data<-subset(data,data$time<=ds2016 | data$time>ds2016+dif)
  #take out duplicate Daylight Savings TimeStamp for 2017
  data<-subset(data,data$time<=ds2017 | data$time>ds2017+dif)
  #take out duplicate Daylight Savings TimeStamp for 2018
  data<-subset(data,data$time<=ds2018 | data$time>ds2018+dif)
  #take out duplicate Daylight Savings TimeStamp for 2018
  data<-subset(data,data$time<=ds2019 | data$time>ds2019+dif)

  #cap the # of rows for .csv file
  data <- tail(data,1000000)
  
  #Determine the length of the dataset
  Data_Length<-as.numeric(nrow(data))
  #extract epoch date & time and convert to normal date & time in string format
  Date_Time<-as.data.frame(anytime(data[,2]))
  #shift the extracted Date_Time up by 1 minute due to the way tradestation reads time stamps
  Date_Time<-as.data.frame(shift(Date_Time[,1],1))
  #get rid of NA at the bottom of Date_Time that was created because of the shift
  Date_Time<-as.data.frame(Date_Time[-Data_Length,])

  #============================================================================
  ################################FORMAT OHLCV#################################
  #============================================================================
  #round volume to nearest whole contract
  Vol<-as.data.frame(round(data[,7],digits=0))
  #get rid of last row as we do not have a time stamp for it anymore
  Vol<-as.data.frame(Vol[-Data_Length,])
  #extract the OHLC data
  OHLC<-select(data,-1,-2,-7)
  #rearrange columns into OHLC format
  OHLC<-OHLC[c("open","high","low","close")]
  ##get rid of last row as we do not have a time stamp for it anymore
  OHLC<-as.data.frame(OHLC[-Data_Length,])

  #============================================================================
  #TIME STAMP DUPES
  #============================================================================
  #Combine the data so we can filter the rows for any dupes based on the time stamp
  csvdata<-cbind(Date_Time,OHLC,Vol)
  names<-c('DATETIME','OPEN','HIGH','LOW','CLOSE','VOLUME')
  colnames(csvdata)<-names
  #get rid of any duplicate time stamps if any
  csvdata<- csvdata[!duplicated(csvdata$DATETIME),]
  #save the OHLC that has the duplicate time stamps taken out
  OHLC<-select(csvdata,-1,-6)
  #save the volume that has the dupes taken out
  Vol<-select(csvdata,-1,-2,-3,-4,-5)

  #============================================================================
  #SEPERATE TIME STAMP TO DATE & TIME
  #============================================================================
  #extract only the date from the string
  Date<-format((as.data.frame(anydate(csvdata[,1]))),"%Y%m%d")
  #extract the Hours and minutes from the normal date & time string
  Hour<-as.data.frame(hour(csvdata[,1]))
  Hour<-as.data.frame(str_pad(Hour[,1],2,pad="0"))
  Min<-as.data.frame(minute(csvdata[,1]))
  Min<-as.data.frame(str_pad(Min[,1],2,pad="0"))
  #Merge Hour and Minutes together
  Time<-as.data.frame(paste0(Hour[,1],Min[,1]))

  #============================================================================
  #SEPERATE TIME STAMP TO DATE & TIME
  #============================================================================
  #re-arrange & Combine the data into one dataframe with a Tradestation friendly format and put headers
  csvdata<-cbind(Date,Time,OHLC,Vol)
  names<-c('DATE','TIME','OPEN','HIGH','LOW','CLOSE','VOLUME')
  colnames(csvdata)<-names

  #============================================================================
  #SAVE DATA TO CSV
  #============================================================================
  file_location <- paste0(mydatafolder,pairdata,"_1min_Coinbase.csv")
  write.csv(csvdata,file_location,row.names=F)
  cat('CSV has been updated!\n')
}
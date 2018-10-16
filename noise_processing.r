#fetch me the plots and the lubridate
library(tidyverse)
library(maps)
library(mapdata)
library(zoo)
library(lubridate)
#install.packages('roll')
library(roll)
#install.packages('xts')
library(xts)

#set working directory to CSVs and grab the list of flights.
setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
#csvFiles <- list.files(path='.', pattern='*.csv')
csvFiles <- c('FLY245.csv', 'FLY246.csv', 'FLY251.csv', 'FLY252.csv', 'FLY253.csv') #FLY254.csv IS LOST bc POM maxed out

for(curFlightCSV in csvFiles){
  setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
  flightData = read.csv(file=curFlightCSV, header = TRUE)
  
  # get GPS info - don't forget to disregard gps height; it's lies.
  thisFlight <- flightData[c(rep(FALSE,4),TRUE), c(8:11, 20:23, 42, 44:49)]
  colnames(thisFlight) <- c('accelX', 'accelY', 'accelZ', 'accelComposite', 'velN', 'velE', 'velD', 'velComposite', 'relativeHeight', 'GPSLong', 'GPSLat', 'GPSDate', 'GPSTime', 'GPSDateTime', 'GPSHeightMSL')
  
  # discard data without a GPS lock,GPS timestamp, or relativeHeight lock
  thisFlight <- thisFlight[!(thisFlight$GPSLong==0 | is.na(thisFlight$GPSLong) | thisFlight$GPSTime == 0 | is.na(thisFlight$relativeHeight)),] 
  
  # fix the timestamps out of LDT-12
  # as it turns out, LDT-12 is Bejing -- which is where DJI is from ... :|
  thisFlight$GPSDateTime <- as.POSIXct(thisFlight$GPSDateTime, format="%Y-%m-%dT%H:%M:%S")-hours(12)
  
  
  # ************************************************
  # *************** pull POM data in ***************
  # ************************************************
  
  #use flight records to get the first/last dates
  date1 <- thisFlight$GPSDateTime[1]
  date2 <- tail(thisFlight$GPSDateTime, 1)
  interv <- interval(date1, date2)
  
  #load the POM data
  setwd ('~/Documents/PDS/OWLETS2/data/POM/')
  flightPOMdf <- read.csv('flt-POMsave_18_06_20_19_52_15.txt', skip=6)
  sfcPOMdf <- read.csv('sfc-POMsave_18_06_20_20_07_17.txt', skip=5)
  # see whether we've got the appropriate POM data
  
  if (nrow(flightPOMdf) > 50){
    #fix the no-header data
    colnames(flightPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
    colnames(sfcPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
    
    #fix the dates into something sortable
    flightPOMdf$datetime <- paste(flightPOMdf$date, flightPOMdf$time)
    flightPOMdf$datetime <- parse_date_time(flightPOMdf$datetime, 'dmy HMS', tz="UTC")
    
    #the time zones are finally compatible, don't fuck with them
    #subset by interval of the current flight
    flightPOMdf <- flightPOMdf[flightPOMdf$datetime %within% interv,]
    
    # right outer join the data. we don't care about drone datapoints without a corresponding POM datapoint
  
    # take the rolling standard deviation
    # https://cran.r-project.org/web/packages/roll/roll.pdf
    # make an XTS object so roll_sd can do this for us
    # https://s3.amazonaws.com/assets.datacamp.com/blog_assets/xts_Cheat_Sheet_R.pdf
    
    flightPOMxts <- xts(x=flightPOMdf$ozone, order.by=flightPOMdf$datetime)
    # calculate standard deviation over 10 seconds.
    flightPOMxts_sd <- roll_sd(flightPOMxts, 10, min_obs = '10')
    flightPOMsddf <- fortify(flightPOMxts_sd)
    flightPOMdf$sd <- flightPOMsddf$flightPOMxts_sd
    
    # calculate mean over 10 seconds, per the reference paper
    flightPOMxts_mean <- roll_mean(flightPOMxts, 10, min_obs = '10')
    flightPOMmeandf <- fortify(flightPOMxts_mean)
    flightPOMdf$mean <- flightPOMmeandf$flightPOMxts_mean
    
    # check each element, see if it's within one standard deviation
    
    # for each row in flightPOMdf
    # if not NA (ignore when we don't have enough stats, for now)
    #   variance <- subtract ozone from mean
    #   numSD <- variance/sd (e.g., how many standard deviations from the mean are we?)
    #   add numSD to the dataframe
    # plot datetime vs numSD
    
    for (obs in 1:nrow(flightPOMdf)){
      variance <- flightPOMdf[obs, 'mean'] - flightPOMdf[obs, 'ozone']
      # the standard variation from reference ozone is .9ppbv over 10s,
      # therefore we take that out.
      # i don't know if this is a good idea buuuut it's easy to undo.
    
      # if (!is.na(variance)){
      #   if(abs(variance) < .9 )
      #   {
      #     variance <- 0
      #   }
      #   else if (variance > 0 ){
      #     variance <- variance - 0.9
      #   }
      #   else if (variance < 0){
      #     variance <- variance + 0.9
      #   } 
      # }
      numSD <- variance / flightPOMdf[obs, 'sd']
      flightPOMdf[obs, 'numSD'] <- numSD
    }
  
    inspirePOMdf <- merge(thisFlight, flightPOMdf, by.y=c("datetime"), by.x=c("GPSDateTime"), all.y = TRUE)
    scatter.smooth(x=inspirePOMdf$velComposite, y=inspirePOMdf$sd, main="vel:sd")
#    scatter.smooth(x=inspirePOMdf$accelComposite, y=inspirePOMdf$sd, main="accel:sd")
  }
  
  # ggplot(flightPOMdf) +
  #   geom_line(aes(x=datetime, y=sd)) +
  #   ylim(-10, 10)
  
  # View(inspirePOMdf)
  # View(flightPOMdf)
  
  
  # https://www.atmos-meas-tech.net/2/125/2009/amt-2-125-2009.pdf
  # fuck me this is actually a phd
  
  #https://pubs.acs.org/doi/pdf/10.1021/ac1013578
  #fuck yes this is the actual POM paper (W&M login required)
  # it claims you can vibrate it a lot but
  # *checks envelope* that was a lie?


  # **** start some mapping here **** 
}


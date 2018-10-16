#fetch me the plots and the lubridate
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(lubridate)

#set working directory to CSVs
setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
# uncomment to do an entire directory
#csvFiles <- list.files(path='.', pattern='*.csv')

csvFiles <- c("FLY310.csv")
for(curFlightCSV in csvFiles){
  setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
  flightData = read.csv(file=curFlightCSV, header = TRUE)
  
  # get GPS info - don't forget to disregard gps height; it's lies.
  thisFlight <- flightData[c(rep(FALSE,4),TRUE), c(42, 44:49)]
  colnames(thisFlight) <- c('relativeHeight', 'GPSLong', 'GPSLat', 'GPSDate', 'GPSTime', 'GPSDateTime', 'GPSHeightMSL')
  
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
  flightPOMdf <- read.csv('pippin1102_20180703b.csv', skip=1)
  # sfcPOMdf <- read.csv('sfc-POMsave_18_06_20_20_07_17.txt', skip=5)
  
  #fix the no-header data
  colnames(flightPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
  # colnames(sfcPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
  
  #fix the dates into something sortable
  flightPOMdf$datetime <- paste(flightPOMdf$date, flightPOMdf$time)
  flightPOMdf$datetime <- parse_date_time(flightPOMdf$datetime, 'dmy HMS', tz="UTC")
  
  #the time zones are finally compatible, don't fuck with them
  #subset by interval of the current flight
  flightPOMdf <- flightPOMdf[flightPOMdf$datetime %within% interv,]
  
  # right outer join the data. we don't care about drone datapoints without a corresponding POM datapoint
  inspirePOMdf <- merge(thisFlight, flightPOMdf, by.y=c("datetime"), by.x=c("GPSDateTime"), all.y = TRUE)
  
  
  # ************************************************ 
  # *************** start the mapping ************** 
  # ************************************************ 

  setwd ('~/Documents/PDS/OWLETS2/data/plotsplotsplots/')

  #make an ozone vs time (alt) plot
  print(qplot (x=inspirePOMdf$GPSDateTime, y=inspirePOMdf$ozone, geom='point', color=inspirePOMdf$relativeHeight) +
    ylim(0,150) +
    scale_colour_gradient(limits = c(0, 150), low='blue', high='green') +
    labs(x = 'GPS Time', y = 'Ozone', title = paste(gsub('.csv', '', curFlightCSV[1]), inspirePOMdf$GPSDateTime[1])) +
    theme_bw()
  )
  ggsave(paste(gsub('.csv', '', curFlightCSV[1]),'-ozone.time.alt.png', sep=''))
  
  #make an alt. vs. time (ozone) plot
  print(
  ggplot(data=NULL, aes(x=GPSDateTime, y=relativeHeight)) +
    geom_point(data=inspirePOMdf, aes(color=ozone)) +
    scale_color_gradient2(limits = c(0, 150), space="Lab", low='blue', mid='yellow', high='red', midpoint=75) +
    labs(color='Ozone', x = 'GPS Time', y = 'Inspire Press. Alt.', title = paste(gsub('.csv', '', curFlightCSV[1]), inspirePOMdf$GPSDateTime[1]))
  )
  #save the plot
  ggsave(paste(gsub('.csv', '', curFlightCSV[1]),'-alt.time.ozone.png', sep=''))
}

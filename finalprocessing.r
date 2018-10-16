#fetch me the plots and the lubridate
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(lubridate)

nmea2dec <- function(coord){
  # thanks to https://stackoverflow.com/questions/8335146/converting-iso-6709-formatted-gps-coordinates-to-decimal-degrees-in-r for this solution at 11:58 p.m. on a monday

  sgns <- sign(coord)
  # this next part is really important, else the math gets weird
  coord <- abs(coord)
  mm <- coord %% 100
  dd <- (coord-mm)/100
  out_coord <- (dd+(mm/60)) * sgns
  return (out_coord)
}

### read all the POM data. ###
setwd ('~/Documents/PDS/OWLETS2/data/POM/')
csvFiles <- list.files(path='.', pattern='*.csv')
POM_all <- do.call(rbind,lapply(csvFiles,read.csv,skip=5,header=FALSE))

#fix the no-header data
colnames(POM_all) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')

#fix the dates into something sortable
POM_all$datetime <- paste(POM_all$date, POM_all$time)
POM_all$datetime <- parse_date_time(POM_all$datetime, 'dmy HMS', tz="UTC")

# POM GPS coordinates are in +/-DDMM.MMM format:
# we have to take it apart and put it back together.
POM_all$gpslat <- nmea2dec(POM_all$gpslat)
POM_all$gpslon <- nmea2dec(POM_all$gpslon)

### ok, let's get all the DRONE DATA ###
# set working directory to CSVs
setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
csvFiles <- list.files(path='.', pattern='*.csv')

# read in the files
flightdata_all <- do.call(rbind,lapply(csvFiles,read.csv,header=TRUE))
# god I hope i exported all the files identically
flightdata_all <- flightdata_all[c(42, 44:49)]
colnames(flightdata_all) <- c('relativeHeight', 'GPSLong', 'GPSLat', 'GPSDate', 'GPSTime', 'GPSDateTime', 'GPSHeightMSL')

# discard data without a GPS lock,GPS timestamp, or relativeHeight lock
flightdata_all <- flightdata_all[!(flightdata_all$GPSLong==0 | is.na(flightdata_all$GPSLong) | flightdata_all$GPSTime == 0 | is.na(flightdata_all$relativeHeight)),] 

# fix the timestamps out of LDT-12
# as it turns out, LDT-12 is Bejing -- which is where DJI is from ... :|
# that's a few hours of my life i'll never get back
  flightdata_all$GPSDateTime <- as.POSIXct(flightdata_all$GPSDateTime, format="%Y-%m-%dT%H:%M:%S")-hours(12)

  #use flight records to get the first/last dates
  d1 <- '2018-06-19 00:00:00'
  d2 <- '2018-06-19 23:59:59'
  date1 <- as.POSIXct(d1, tz="America/New_York", format="%Y-%m-%d %H:%M:%S")
  date2 <- as.POSIXct(d2, tz="America/New_York", format="%Y-%m-%d %H:%M:%S")
  interv <- interval(date1, date2)
  
  #the time zones are finally compatible, don't fuck with them
  #subset by interval of the current day
  
  POM_today <- POM_all[POM_all$datetime %within% interv,]
  flightdata_today <- flightdata_all[flightdata_all$GPSDateTime %within% interv,]
  
  # right outer join the data. we don't care about drone datapoints without a corresponding POM datapoint

  
  View(POM_today)
  View(flightdata_today)
  View(inspirePOMdf)
  # ok, now we need to start shifting the data back and forth to make sure we've actually got something useful.

  # so here's how we're gonna do it.

  
    
  # ************************************************ 
  # *************** start the mapping ************** 
  # ************************************************ 
  # 
  # #make an ozone vs time (alt) plot
  # print(qplot (x=inspirePOMdf$GPSDateTime, y=inspirePOMdf$ozone, geom='point', color=inspirePOMdf$relativeHeight) +
  #   ylim(0,125) +
  #   scale_colour_gradient(limits = c(20, 125), low='blue', high='green') +
  #   labs(x = 'GPS Time', y = 'Ozone', title = paste(gsub('.csv', '', curFlightCSV[1]), inspirePOMdf$GPSDateTime[1])) +
  #   theme_bw()
  # )
  # ggsave(paste(gsub('.csv', '', curFlightCSV[1]),'-ozone.time.alt.png', sep=''))
  # 
  # #make an alt. vs. time (ozone) plot
  # print(
  # ggplot(data=NULL, aes(x=GPSDateTime, y=relativeHeight)) +
  #   geom_point(data=inspirePOMdf, aes(color=ozone)) +
  #   scale_color_gradient2(limits = c(20, 125), space="Lab", low='blue', mid='yellow', high='red', midpoint=55) +
  #   labs(color='Ozone', x = 'GPS Time', y = 'Inspire Press. Alt.', title = paste(gsub('.csv', '', curFlightCSV[1]), inspirePOMdf$GPSDateTime[1]))
  # )
  # #save the plot
  # ggsave(paste(gsub('.csv', '', curFlightCSV[1]),'-alt.time.ozone.png', sep=''))
  # 

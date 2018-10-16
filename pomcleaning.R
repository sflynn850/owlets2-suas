  #fetch me the breastplate stretcher
  library(ggplot2)
  library(ggmap)
  library(maps)
  library(mapdata)
  library(lubridate)
  
  # THIS IS IN LDT FOR SOME DUMB REASON
  date1 <- as.POSIXct("2018-06-20 12:40:00")
  date2 <- as.POSIXct("2018-06-20 16:59:59")
  
  setwd ('~/Documents/PDS/OWLETS2/data/POM/')
  
  flightPOMdf <- read.csv('flt-POMsave_18_06_20_19_52_15.txt', skip=6)
  sfcPOMdf <- read.csv('sfc-POMsave_18_06_20_20_07_17.txt', skip=5)
  
  #fix the no-header data
  colnames(flightPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
  colnames(sfcPOMdf) <- c('row','ozone','celltemp','cellpress','pdvolt','psvolt','gpslat','gpslon','gpsalt','gpsqual','date','time')
  
  #fix the dates into something sortable
  flightPOMdf$datetime <- paste(flightPOMdf$date, flightPOMdf$time)
  flightPOMdf$datetime <- parse_date_time(flightPOMdf$datetime, 'dmy HMS')
  #something fucky is going on with time zones here -- beware.
  interv <- interval(date1, date2)
  flightPOMdf <- flightPOMdf[flightPOMdf$datetime %within% interv,]
  
  qplot (x=flightPOMdf$datetime, y=flightPOMdf$ozone, geom='point', color=flightPOMdf$gpsalt) +
    ylim(0,150) +
    scale_colour_gradient(limits = c(0, 200), low='blue', high='green') +
    theme_bw()
  
  # 
  # #************************************************#
  # # let's do it again, but for the 'surface' POM.
  # 
  # #fix the dates into something sortable
  # sfcPOMdf$datetime <- paste(sfcPOMdf$date, sfcPOMdf$time)
  # sfcPOMdf$datetime <- parse_date_time(sfcPOMdf$datetime, 'dmy HMS')
  # #something fucky is going on with time zones here -- beware.
  # 
  # 
  # interv <- interval(date1, date2)
  # sfcPOMdf <- sfcPOMdf[sfcPOMdf$datetime %within% interv,]
  # 
  # qplot (x=sfcPOMdf$datetime, y=sfcPOMdf$gpsalt, geom='point', color=sfcPOMdf$ozone) +
  #   ylim(0,200) +
  #   scale_colour_gradient(limits = c(30, 100), low='white', high='black') +
  #   theme_minimal()
  # 
  # qplot (x=sfcPOMdf$datetime, y=sfcPOMdf$ozone, geom='point', color=sfcPOMdf$gpsalt) +
  #   ylim(0,150) +
  #   scale_colour_gradient(limits = c(10, 150), low='blue', high='green') +
  #   theme_bw()
  # 
  # 
  # ## combo plots
  # 
  
  ggplot(data=NULL, aes(x=datetime, y=gpsalt)) +
    geom_point(data=flightPOMdf, aes(color=ozone)) +
    # scale_color_gradient(limits =c(30, 75), low='white', high='red')
    scale_color_gradient2(limits =c(60, 130), space="Lab", low='blue', mid='yellow', high='red', midpoint=90)
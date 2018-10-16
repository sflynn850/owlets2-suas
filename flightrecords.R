#fetch me the plots and the lubridate
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(lubridate)

register_google(key='AIzaSyD3VzY14_GNyPrusTsYmbWulWBul4ADXMg')

#set working directory to CSVs
setwd ('~/Documents/PDS/OWLETS2/data/flightdat/CSVs/')
csvFiles <- list.files(path='.', pattern='*.csv')

#csvFiles <- c("FLY308.csv")
for(curFlightCSV in csvFiles){
  flightData = read.csv(file=curFlightCSV, header = TRUE)
  
  # get GPS info — disregard gps height; it's lies.
  thisFlight <- flightData[c(rep(FALSE,4),TRUE), c(42, 44:49)]
  colnames(thisFlight) <- c('relativeHeight', 'GPSLong', 'GPSLat', 'GPSDate', 'GPSTime', 'GPSDateTime', 'GPSHeightMSL')

  # discard data without a GPS lock,GPS timestamp, or relativeHeight lock
  thisFlight <- thisFlight[!(thisFlight$GPSLong==0 | is.na(thisFlight$GPSLong) | thisFlight$GPSTime == 0 | is.na(thisFlight$relativeHeight)),] 

    # fix the timestamps
  thisFlight$GPSDateTime <- as.POSIXct(thisFlight$GPSDateTime, format="%Y-%m-%dT%H:%M:%S")-hours(12)
  
  sbbox <- make_bbox(lon = thisFlight$GPSLong, lat = thisFlight$GPSLat, f = 1.1)
  ll_means <- sapply(thisFlight[2:3], mean)
  sq_map2 <- get_map(location = sbbox,  maptype = "satellite", source = "google")

  #make the map
    ggmap(sq_map2) + 
      geom_point(data = thisFlight, aes(x=GPSLong, y=GPSLat, color=relativeHeight), size = 1) +
      scale_color_gradient2(limits = c(0, 150), low='blue', mid='yellow', midpoint = 120, high='red') +
      labs(x = 'Inspire-GPS Lat', y = 'Inspire-GPS Long', color = 'Inspire-Pressure Alt. (m)', title = paste(gsub('.csv', '', curFlightCSV[1]), paste(sept=' ', thisFlight$GPSDateTime[1]), 'LDT'))
    

  #save the plot
  ggsave(paste(gsub('.csv', '', curFlightCSV[1]),'.png', sep=''))
}

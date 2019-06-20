library(ncdf4)

setwd("C:/Users/MariaPaquin/Documents")

#*[-----------------------------------------------------------------------------------------]*#
#*[ Objective: This code opens and reads PERSIANN-CDR netCDF files and outputs txt files    ]*#
#*[-----------------------------------------------------------------------------------------]*#

year.first <- 1983 
year.last <- 2015

days.in.month.365<-c(31,28,31,30,31,30,31,31,30,31,30,31)
days.in.month.366<-c(31,29,31,30,31,30,31,31,30,31,30,31)

x <- 0 # error checking
precip.df <- data.frame()

for(i.year in year.first:year.last){
  total.days<-ifelse((i.year%%4==0 & i.year%%100!=0) | i.year%%400==0,366,365)
  
  if (total.days==365) {
    days.in.month <- days.in.month.365
    days.in.year <- c(1:365)
  } else {
    days.in.month <- days.in.month.366
    days.in.year <- c(1:366)
  }
  
  for(i.month in 1:12){
    
    for (i.day in 1:days.in.month[i.month]) {
      
      x <- x+1
      print(paste("day", x, sep = " ")) # error checking
      
      date <- as.numeric(sprintf("%i%02i%02i", i.year, i.month, i.day))
      
      ncin <- nc_open(
        Sys.glob(file.path("Data/", i.year, "/PERSIANN-CDR_v01r01_", date, "*.nc", fsep = "")))
      
      if(i.day == 1 && i.month == 1){ # Retrieving the latlon columns only once
        
        lat <- ncvar_get(ncin, "lat")
        lon <- ncvar_get(ncin, "lon")
        
        latlon <- expand.grid(lat, lon) # 691,200 X 2
        
        precip.df <- latlon
      }
      
      precip.array <- ncvar_get(ncin, "precipitation")
      precip.vec <- round(as.vector(precip.array),5) # 691,200 X 1
      
      precip.df <- data.frame(cbind(precip.df, precip.vec))
      
      nc_close(ncin)
    }
    
  }
  
  dates = format(seq(from = as.Date(paste(i.year, "-01-01", sep = "")),
           to = as.Date(paste(i.year, "-12-31", sep = "")),
           by = "day"), "%b %d")
  
  colnames(precip.df) <-  c("Lat", "Lon", dates)
  
  write.table(precip.df,
              file=paste("D:/output/daily/Step1_DailyPPT_", i.year, ".txt", sep=""), 
              append=FALSE, row.names = FALSE, col.names = TRUE)
  
}
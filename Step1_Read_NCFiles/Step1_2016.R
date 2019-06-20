library(ncdf4)

setwd("C:/Users/MariaPaquin/Documents")

i.year <- 2016

days.in.month<-c(31,29,31,30,31,30,31,31)

precip.df <- data.frame()
  
for(i.month in 1:8){
  
  x <- 0 # error checking
    
  for (i.day in 1:days.in.month[i.month]) {
      
    x <- x + 1
    print(paste("day", x, sep = " ")) # error checking
      
    date <- as.numeric(sprintf("%i%02i%02i", i.year, i.month, i.day))
      
    ncin <- nc_open(Sys.glob(file.path("Data/", i.year, "/PERSIANN-CDR_v01r01_", date, "*.nc", fsep = "")))
      
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

dates = format(seq(from = as.Date(paste("2016-01-01", sep = "")),
                   to = as.Date(paste("2016-08-31", sep = "")),
                   by = "day"), "%b %d")

colnames(precip.df) <-  c("Lat", "Lon", dates)
  
write.table(precip.df,file=paste("D:/output/daily/Step1_DailyPPT_", i.year, ".txt", sep=""), append=FALSE, row.names = FALSE, col.names = TRUE)

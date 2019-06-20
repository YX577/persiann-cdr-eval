setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This R program generates PERSIANN-CDR seasonal mean data by doing the following:   ]*#
#*[            (1) Read in Indices (691,200)                                                      ]*#
#*[            (2) For each season in each year, read in seasonal data,                           ]*#
#*[                and for each index, find mean of that season if less than 15% of values        ]*#
#*[                are missing, or else mark that seasonal mean as missing.                       ]*#
#*[ Author: Maria Paquin                                                                          ]*#
#*[ Updated: 08/17/2017                                                                           ]*#
#*[                                                                                               ]*#
#*[-----------------------------------------------------------------------------------------------]*#

seasons <- c("Winter", "Spring", "Summer", "Fall")

seasonalMean <- read.table(file="LatLon.txt", header = TRUE)

index <- 3

for(i.year in 1983:2016){
  
  print(i.year)
  
  for(i.season in 1:4){
    
    if((i.year == 1983 && i.season == 1) || (i.year == 2016 && i.season == 4)){ # No data for Winter 1983 or Fall 2016
      
      seasonalMean[,index] <- NA
      
    }else{
      
      df <- read.table(file=paste("SeasonalData/", i.year, "/", seasons[i.season], ".txt", sep = ""), header = TRUE)
      
      my.mean <- function(x) {
        ifelse(sum(is.na(x)) <= (0.15*ncol(df)), mean(x, na.rm=TRUE), NA)
      } 
      
      seasonalMean[,index] <- apply(df, 1, my.mean)
    }
    
    index <- index + 1
  }
}

colnames(seasonalMean) <- c("Lat", "Lon", paste(rep(c("Winter","Spring","Summer","Fall"), 34), rep(c(1983:2016), each = 4), sep = " "))

write.table(seasonalMean, file=paste("D:/output/seasonalMean/seasonalMean.txt", sep = ""), append = FALSE,  row.names = FALSE, col.names = TRUE)

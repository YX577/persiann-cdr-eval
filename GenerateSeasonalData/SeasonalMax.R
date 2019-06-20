setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This R program generates PERSIANN-CDR seasonal max data by doing the following:    ]*#
#*[            (1) Read in Indices (691,200)                                                      ]*#
#*[            (2) For each season in each year, read in seasonal data,                           ]*#
#*[                and for each index, find max of that season if less than 5% of values          ]*#
#*[                are missing, or else mark that seasonal max as missing.                        ]*#
#*[ Author: Maria Paquin                                                                          ]*#
#*[ Updated: 06/4/2017                                                                            ]*#
#*[                                                                                               ]*#
#*[-----------------------------------------------------------------------------------------------]*#

seasons <- c("Winter", "Spring", "Summer", "Fall")

seasonalMax <- read.table(file="LatLon.txt", header = TRUE)

index <- 3

for(i.year in 1983:2016){
  
  print(i.year)
  
  for(i.season in 1:4){

    if((i.year == 1983 && i.season == 1) || (i.year == 2016 && i.season == 4)){ # No data for Winter 1983 or Fall 2016
      
      seasonalMax[,index] <- NA

    }else{
      
      df <- read.table(file=paste("seasonal/", i.year, "/", seasons[i.season], ".txt", sep = ""), header = TRUE)
      
      my.max <- function(x) {
        ifelse(sum(is.na(x)) <= (0.05*ncol(df)), max(x, na.rm=TRUE), NA) # about 4.5
      } 
      
      seasonalMax[,index] <- apply(df, 1, my.max)
    }
    
    index <- index + 1
  }
}

colnames(seasonalMax) <- c("Lat", "Lon", paste(rep(c("Winter","Spring","Summer","Fall"), 34), rep(c(1983:2016), each = 4), sep = " "))

write.table(seasonalMax, file=paste("D:/output/seasonalMax/seasonalMax.txt", sep = ""), append = FALSE,  row.names = FALSE, col.names = TRUE)

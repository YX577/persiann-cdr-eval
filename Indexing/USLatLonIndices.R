setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ This program reads in the lat and lon indices and writes out the indices of the US            ]*#
#*[ (approximate rectangle around US)                                                             ]*#
#*[-----------------------------------------------------------------------------------------------]*#


df.latlon <- read.table(file="LatLon.txt", header = TRUE)


US <- df.latlon[which(df.latlon$Lon > 235 & df.latlon$Lon < 290 &
                                 df.latlon$Lat > 25 & df.latlon$Lat < 50),]


write.table(US,file="D:/output/USLatLon.txt", append=FALSE, row.names = TRUE, col.names = FALSE)

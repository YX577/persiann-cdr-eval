setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ This program reads in the lat and lon indices and writes out the indices without the first 2  ]*#
#*[ and last 2 rows.                                                                              ]*#
#*[-----------------------------------------------------------------------------------------------]*#

df.latlon <- read.table(file="LatLon.txt", header = TRUE)

df.sans4rows <-  df.latlon[df.latlon[, "Lat"] != 59.875
                        & df.latlon[,"Lat"] != 59.625
                        & df.latlon[,"Lat"] != -59.875 
                        & df.latlon[,"Lat"] != -59.625,]

write.table(df.sans4rows,file="D:/output/LatLonSansEdges.txt", append=FALSE, row.names = TRUE, col.names = FALSE)

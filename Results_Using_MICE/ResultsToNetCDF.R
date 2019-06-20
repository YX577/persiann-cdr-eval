setwd("D:/output/")

library("ncdf4")

#*[----------------------------------------------------------------------------------------]*#
#*[ Objective: This code reads in the text file containing the regression coefficients     ]*#
#*[            and uses the time variable coefficent to create a netCDF file plot which    ]*#
#*[            can be viewed with Panoply.                                                 ]*#
#*[-----------------------------------------------------------------------------------------]*#

# Find and replace mean/median

results <- read.table(file = "global_mean_results/mean_output.txt", header = FALSE)
colnames(results) <- c("LAT", "LON", "INDEX", "Int.est", "Q2.est", "Q3.est", 
                       "Q4.est", "Time.est", "Int.se", "Q2.se", "Q3.se", "Q4.se", "Time.se")
precip <- as.matrix(rep(NA,691200))

precip[as.matrix(results$INDEX),] <- as.matrix(results$Time.est)
# precip[as.matrix(results$INDEX),] <- as.matrix(results$Time.est/results$Time.se)
# precip[is.nan(precip)] <- 0

lon <- seq(from = 0.125, to = 359.875, by = 0.25)
lat <- seq(from = 59.875,
           to = -59.875,
           by = -0.25)

dim1 <- ncdim_def(name = "lat", units = "degrees_north", longname = "latitude", vals = lat)
dim2 <- ncdim_def(name = "lon", units = "degrees_east", longname = "longitude", vals = lon)

var <- ncvar_def(
  name = "precip",
  units =  "millimeters",
  dim = list(dim1, dim2),
  missval = NA,
  longname = "Trends in Seasonal Mean Precipitation"
)

ncnew <- nc_create( "D://output/global_mean_results/mean_output.nc", var)
ncvar_put(ncnew, var, precip)

nc_close(ncnew)

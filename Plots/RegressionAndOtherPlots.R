setwd("D:/output/")

#*[------------------------------------------------------------------------------------------------]*#
#*[ Objective: This program reads PERSIANN-CDR model seasonal (mean, median or max) precipitation  ]*#
#*[            data and does the following given lat & lon corrdinates:                            ]*#
#*[                                                                                                ]*#
#*[            1) Creates a Boxplot of the time series                                             ]*#
#*[            2) Fits a regression model (ignoring missing values)                                ]*#
#*[            3) Creates a time series plot with regression fit                                   ]*#
#*[            4) Creates plot of residulas, ACF and PACF                                          ]*#
#*[------------------------------------------------------------------------------------------------]*#

#####################
# Time Series Plot  #
#####################

# Read in seasonal data

df <- read.table(file="seasonal_mean/seasonalMean.txt", header = TRUE)

# location <- "Atlanta"
# lat <- 33.625
# lon <- 275.625

# location <- "Boise"
# lat <- 43.625
# lon <- 243.625

# location <- "Boston"
# lat <- 42.375
# lon <- 288.875

# location <- "Kansas_City"
# lat <- 39.125
# lon <- 265.375

# location <- "LA"
# lat <- 34.125
# lon <- 241.875

# location <- "Orlando"
# lat <- 28.375
# lon <- 278.625

# location <- "Phoenix"
# lat <- 33.375
# lon <- 247.875

# location <- "Richmond"
# lat <- 37.625
# lon <- 282.625

# location <- "San Francisco"
# lat <- 37.625
# lon <- 237.625

# location <- "Saint_Paul"
# lat <- 44.875
# lon <- 266.875

# Extract grid point to be analyzed
pt <- with(df, df[ Lat == lat & Lon == lon, c(-1,-2)])
pt <- t(pt)
write.table(pt, file= paste("D:/output/time_series_mean/", location, sep = ""), 
            append = FALSE,  row.names = FALSE, col.names = FALSE)

pt.ts<-ts(pt, frequency = 4, start = c(1983,1))

# TIME SERIES PLOT
dev.new()
plot.ts(pt.ts,xlab="Year",ylab="Millimeters",
        main=paste(location, "Precipitation,  Winter 1983 - Summer 2016"))

# BOXPLOT BY SEASON
pt.matrix<-matrix(pt.ts,ncol=4,byrow=TRUE)
dev.new(width=8,height=5)
boxplot(split(pt.matrix,col(pt.matrix)),xaxt = "n", ylab="Millimeters",
        main=paste(location, "Mean Boxplot by Season"))
axis(1, at = 1:4, labels = c("Winter", "Spring","Summer","Fall"))


#######################
# REGRESSION ANALYSIS #
#######################

n<- length(pt)
time <- (1:n)/40

Q1 = rep(c(1, 0, 0, 0), n / 4)
Q2 = rep(c(0, 1, 0, 0), n / 4)
Q3 = rep(c(0, 0, 1, 0), n / 4)
Q4 = rep(c(0, 0, 0, 1), n / 4)

fit.ols <- lm(pt.ts ~ Q1 + Q2 + Q3 + Q4 + time -1, na.action=na.exclude)

s <- summary(fit.ols)
capture.output(s, file = paste("D:/output/time_series_mean/", location, "Summary", sep = ""))

prd.ols <- ts(fitted(fit.ols),start=1983, freq = 4)
rsd.ols<-pt.ts-prd.ols

dev.new(width = 9, height = 4)
par(mfrow=c(1,1), mex = .8)
plot.ts(pt.ts,xlab = "Year", ylab="Milimeters",main= paste(location, "Precipitation and OLS fit"))

lines(prd.ols,col="orange")

dev.new()
par(mfrow=c(3,1), mex = .9)

plot.ts(rsd.ols,xlab="Year",ylab="Milimeters",main= "OLS Residuals")
abline(h = mean(rsd.ols, na.rm = TRUE), col = "blue", lty = "dashed")
acf(rsd.ols,na.action = na.pass, main = "ACF")
pacf(rsd.ols,na.action = na.pass, main = "PACF")

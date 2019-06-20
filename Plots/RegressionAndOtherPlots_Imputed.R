setwd("D:/output/")

library("mice")
library("lattice")

#*[------------------------------------------------------------------------------------------------]*#
#*[ Objective: This program reads PERSIANN-CDR model seasonal (mean, median or max) precipitation  ]*#
#*[            data and does the following:                                                        ]*#
#*[                                                                                                ]*#
#*[            1) Imputes missing values with MICE algorithm                                       ]*#
#*[            2) Fits a regression model                                                           ]*#
#*[            3) Creates a time series plot with regression fit                                   ]*#
#*[            4) Creates plot of residulas, ACF and PACF                                          ]*#
#*[------------------------------------------------------------------------------------------------]*#

# Find and replae mean/median/max

# df <- read.table(file="SeasonalMedian/seasonalMedian.txt", header = TRUE)
 
# location <- "Unknown1"
# lat <- -49.125
# lon <- 80.125
# 
location <- "Lat -50.125, Lon 90.125"
lat <- -50.125
lon <- 90.125

# location <- "Unknown3"
# lat <- -50.125
# lon <- 85.125

# location <- "Boise"
#lat <- 43.625
# lon <- 243.625

# lat & lon of surrounding locations
Y = matrix(c(lat+.25,lon-.25,lat+.25,lon,lat+.25,lon+.25,lat,lon-.25,
             lat,lon,lat,lon+.25,lat-.25,lon-.25,lat-.25,lon,lat-.25,lon+.25)
           ,nrow = 9, ncol = 2, byrow = TRUE)

rownames(Y) <- paste("Y", c(1:9), sep="")
colnames(Y) <- c("latitude","longitude")

point.df <- NULL

for(i in 1:9){
  
  lat <- Y[paste("Y", i, sep = ""), "latitude"]
  lon <- Y[paste("Y", i, sep = ""), "longitude"]
  
  y <- as.numeric(with(df, df[ Lat == lat & Lon == lon, c(-1,-2)]))
  point.df <- cbind(point.df, y)
  
  # if(i == 5){
  #   point.ts <- ts(y, frequency = 4, start = c(1983,1))
  #   point <- y
  #   write.table(point, file= paste("D:/output/ChangePoints/Median_regression_output/", 
  #                            location, sep = ""), 
  #               append = FALSE,  row.names = FALSE, col.names = FALSE)
  # }
} 

colnames(point.df) <- paste("Y", c(1:9), sep="")


#######################
# REGRESSION ANALYSIS #
#######################

n<- nrow(point.df)
time <- (1:n)/40

Q1 = rep(c(1, 0, 0, 0), n / 4)
Q2 = rep(c(0, 1, 0, 0), n / 4)
Q3 = rep(c(0, 0, 1, 0), n / 4)
Q4 = rep(c(0, 0, 0, 1), n / 4)

One <- Q1+Q2+Q3+Q4

Q <- Q1 + 2*Q2 + 3*Q3 + 4*Q4

point.df <- cbind(point.df, Q, time)

#############
#   MICE    #
#############

# (1) mice() to create imputations

ini <- mice(point.df, maxit = 0)
pred <- ini$predictorMatrix


pred[1:9,"Q"] <- -2


tempData <- mice(point.df, method = c("2l.norm2","2l.norm2","2l.norm2",
                                      "2l.norm2","2l.norm2","2l.norm2",
                                      "2l.norm2","2l.norm2","2l.norm2","",""),
                 predictorMatrix = pred, m=15, seed=500)

# (2) with() to estimate regression coefficients of each imputed dataset

fit <- with(tempData,lm(Y5 ~ factor(Q) + time))

# densityplot(tempData) shows density of observed and estimated values

# (3) pool() to combine results

p.fit <- summary(pool(fit))

capture.output(p.fit, file = paste("D:/output/ChangePoints/Summary", sep = ""))

##################
# Regression fit #
##################

prd.ols <- ts(cbind(One,Q2,Q3,Q4,time)%*%as.numeric(p.fit[,"est"]), start=1983, freq = 4)


# Residuals
rsd.ols<-point.ts-prd.ols


# Residuals, ACF, PACF
dev.new(width = 9, height = 4)
par(mfrow=c(1,1), mex = .8)
plot.ts(point.ts,xlab = "Year", ylab="Millimeters",ylim=c(0, 3),
        main= paste(location, " Seasonal Median Precipitation with Regression Model", sep = ""),
        cex.lab=2, cex.axis=2, cex.main=1.5, cex.sub=2)
lines(prd.ols,col="orange")
ind <- 1983 + 0:33
legend("topright", lty = c("solid", "solid"), legend = c("Actual", "Predicted"), 
       col = c("black", "orange"))

png(filename = paste("D:/output/ChangePoints/Median_regression_output/ 
                    Resid.ACF.png", sep = ""),width=5,height=6,units="in",res=100)
par(mfrow=c(3,1), mex = .9)
plot.ts(rsd.ols,xlab="Year",ylab="Milimeters",main= "Regression Residuals")
abline(h = mean(rsd.ols, na.rm = TRUE), col = "blue", lty = "dashed")
acf(rsd.ols,na.action = na.pass, main = "ACF")
pacf(rsd.ols,na.action = na.pass, main = "PACF")
dev.off()

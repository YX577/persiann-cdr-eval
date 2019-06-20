setwd("D:/output/")

#*[-------------------------------------------------]*#
#*[                                                 ]*#
#*[ COMPARING PERSIANN-CDR DAILY DATA TO NOAA DAILY ]*#
#*[ DATA IN BOISE AREA IN 2005                      ]*#
#*[                                                 ]*#
#*[-------------------------------------------------]*#

# READ IN 2005 SEASONAL MAX PRECIPITATION DATA
df.winter <- read.table(file=paste("SeasonalData/2005/Winter.txt", sep = ""), header = TRUE)
df.spring <- read.table(file=paste("SeasonalData/2005/Spring.txt", sep = ""), header = TRUE)
df.summer <- read.table(file=paste("SeasonalData/2005/Summer.txt", sep = ""), header = TRUE)
df.fall <- read.table(file=paste("SeasonalData/2005/Fall.txt", sep = ""), header = TRUE)

latlon <- read.table(file=paste("MissingYearly/1983_Missing.txt", sep = ""), header = TRUE)

# BOISE LAT & LON
lat <- 43.625
lon <- 243.625

# EXTRACT BOISE ROW FROM EACH SEASONAL DATA FRAME
winter <- cbind(latlon[, 1:2], df.winter)
spring <- cbind(latlon[, 1:2], df.spring)
summer <- cbind(latlon[, 1:2], df.summer)
fall <- cbind(latlon[, 1:2], df.fall)

boise.winter <- with(winter, winter[ Lat == lat & Lon == lon, c(-1,-2)])
boise.spring <- with(spring, spring[ Lat == lat & Lon == lon, c(-1,-2)])
boise.summer <- with(summer, summer[ Lat == lat & Lon == lon, c(-1,-2)])
boise.fall <- with(fall, fall[ Lat == lat & Lon == lon, c(-1,-2)])

# PLOT PERSIANN CDR (PURPLE) AND NOAA (BLACK) DATA FOR EACH SEASON
dev.new(width=50,height=25,noRStudioGD = TRUE) 
par(mfrow=c(2,2),xpd = FALSE, oma=c(1.2,1.7,5,2), mar = c(1.7,4,2,1.5))

date.winter <- seq(as.Date("2004-12-01"), as.Date("2005-02-28"), by="days")
date.spring <- seq(as.Date("2005-03-01"), as.Date("2005-05-31"), by="days")
date.summer <- seq(as.Date("2005-06-01"), as.Date("2005-08-31"), by="days")
date.fall <- seq(as.Date("2005-09-01"), as.Date("2005-11-30"), by="days")

plot(date.winter, boise.winter, main = "Winter", xlab = "Date", ylab = "Precipitation (mm)", type = "l", col = "steelblue3", cex.lab=1.3, cex.axis=1.2, cex.main=1.2)
winter.precip <- c(0,0,0,0,0,.01,0,.66, .4, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.15,.01,0,.02,0,0,.01,0,0,0,.01,0,0,0,0,0,0,.02,.04,0,.02,0,0,0,0,0,0,0,0,0,.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.01,.14,0,0,0,0,0,.16,.01,0,0,0,0,0,0,0,0)
winter.precip <- winter.precip*25.4

lines(date.winter, winter.precip, type = "l")

legend("topright", lty = c("solid", "solid"), legend=c("PERSIANN-CDR", "CPC"),col=c("steelblue3", "black"))


plot(date.spring, boise.spring, main = "Spring", xlab = "Date", ylab = "Precipitation (mm)", type = "l", col = "steelblue3", cex.lab=1.3, cex.axis=1.2, cex.main=1.2)
spring.precip <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.04,0,0.11,0.08, 0,0.10,0.13,0,0,0.01,0.09,0.31,0.37,0,0, 0,0,0,0.21,0,0,0.09, 0.01,0,0,0,0,0.03,0,0,0,0.01,0.02,0,0.02,0.41, 0, 0.16, 0.18, 0,0,0.04,0,0,0,0,0,0,0.03,0.88, 0.26, 0.06,0.06,0.45,0,0.05,0,0,0,0.18,0.81, 0.07, 0.06,0.08,0.07,0,0,0,0,0,0,0,0,0.40,0,0.57)
spring.precip <- spring.precip*25.4

lines(date.spring, spring.precip, type = "l")

plot(date.summer, boise.summer, main = "Summer", xlab = "Date", ylab = "Precipitation (mm)", ylim = c(0, 10), type = "l", col = "steelblue3", cex.lab=1.3, cex.axis=1.2, cex.main=1.2)
summer.precip <- c(NA,0,0,0,.03,0,0,0,0,0,0,0,0,0,0,0.03,0.41,.1,0,0,0,0,0,0,0,0,.32,.02,0,0,0,0,0,0,0,0,0,0,.02,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,NA,0,0,0,NA,0,0,0,0,0,0,0,0,0)
summer.precip <- summer.precip*25.4

lines(date.summer, summer.precip, type = "l")

plot(date.fall, boise.fall, main = "Fall", xlab = "Date", ylab = "Precipitation (mm)", type = "l", col = "steelblue3", cex.lab=1.3, cex.axis=1.2, cex.main=1.2)
fall.precip <- c(0,0,0,0,0,0,0,0,.04,0,0,0,0,0,0,.14,.13,0,0,0,0,0,NA,.01,0,0,0,0,0,0,.19,.01,.03,0,0,0,0,.02,.02,0,.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,.07,NA,NA,0,0.01,0,.02,.21,.04,.07,.48,.03,.05,0,0,0,NA,.09,.06,0,0,0,0,0,0,0,0,NA,0,.57,.02,0,NA,.04,0)
fall.precip <- fall.precip*25.4

lines(date.fall, fall.precip, type = "l")

# title(main="Comparison of CPC and PERSIANN-CDR Daily Precipitation Data by Season",outer=T)

mtext(text="Boise Daily Precipitation in 2005",side = 3, line = 3, outer = TRUE, font = 2, cex = 1.7)

# https://www.wunderground.com/history/airport/KBOI/2017/06/04/DailyHistory.html?req_city=Boise&req_state=ID&reqdb.zip=&reqdb.magic=&reqdb.wmo=
# http://www.wrh.noaa.gov/climate/monthdisp.php?stn=KBOI&year=2005&mon=6&wfo=boi
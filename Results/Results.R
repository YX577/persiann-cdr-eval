library("plyr")
library("foreach")
library("doParallel")

source("~/ChangePoints.R")

#*[----------------------------------------------------------------------------------------------------]*#
#*[ Objective: This R code reads in PERSIANN-CDR seasonal mean/median data, and for each               ]*#
#*[            grid point:                                                                             ]*#
#*[                                                                                                    ]*#
#*[           1) Detects changepoints in seasonal data                                                 ]*#
#*[                                                                                                    ]*#
#*[           2) Fits 2 regression models to the data, one including any changepoints found            ]*#
#*[                                                                                                    ]*#
#*[           3) Outputs coeffieients and std errors of the two regression models. If changepoints     ]*#
#*[              are found, locations of the season(s) they occur are output to another file.          ]*#
#*[                                                                                                    ]*#
#*[ Output: Lat Lon Index Int.est Q2.est Q3.est Q4.est Time.est Int.se Q2.se Q3.se Q4.se Time.se NumCP ]*#
#*[         (14 columns for each grid point)                                                           ]*#
#*[                                                                                                    ]*#
#*[         Lat Lon Index Int.est Q2.est Q3.est Q4.est Time.est Int.se Q2.se Q3.se Q4.se Time.se       ]*#
#*[         (13 columns for each grid point)                                                           ]*#
#*[                                                                                                    ]*#
#*[         Lat Lon Index Changepoint.Locations                                                        ]*#
#*[         (Only for grid points with changepoints detected)                                          ]*#
#*[                                                                                                    ]*#
#*[ THIS CODE IS WRITTEN TO BE RUN IN PARALLEL ON THE CLUSTER R2                                       ]*#
#*[                                                                                                    ]*#
#*[ Author: Maria Paquin                                                                               ]*#
#*[ Updated: 01/26/2018                                                                                ]*#
#*[----------------------------------------------------------------------------------------------------]*#

cores <- detectCores()
cluster <- makeCluster(cores)     # Reserve one core for other use
registerDoParallel(cluster)

df <- read.table(file="~/data/seasonalMean.txt", header = TRUE)

global.index1 <- read.table(file="~/index/global.index1.txt", header = TRUE)
global.index1 <- as.matrix(global.index1)

# Variables for regression model
n <- 136
time <- (1:n)/40
Q1 = rep(c(1, 0, 0, 0), n / 4)
Q2 = rep(c(0, 1, 0, 0), n / 4)
Q3 = rep(c(0, 0, 1, 0), n / 4)
Q4 = rep(c(0, 0, 0, 1), n / 4)
One <- Q1+Q2+Q3+Q4
Q <- Q1 + 2*Q2 + 3*Q3 + 4*Q4

# Function to get seasonal data for one grid point from data set
getSeasData <- function(Y){
  y.lat <- Y[1]
  y.lon <- Y[2]
  seasData <- as.matrix(with(df, df[ Lat == y.lat & Lon == y.lon, ]))
  return(seasData)
}

# Function to calculate regression coefficients 
# Regression model including any changepoints found
# Ignoring any missing values.
trendAnalysis <- function(row.index){
  
  index <- as.numeric(row.index[1])
  lat <- as.numeric(row.index[2])
  lon <- as.numeric(row.index[3])
  
  Y <- cbind(lat,lon)
  point <- getSeasData(Y)
  point <- as.matrix(point[,c(-1,-2)])
  cp_config <- changepoints(point)
  
  numCP = cp_config[1]
  
  if(numCP != 0){
    cp = cp_config[-1]
    k = cp_config[1]
    
    delta <- matrix(0, nrow=136,ncol=1+k)
    
    if(k >= 2){
      for (i in 2:k){
        delta[,i] <- c(rep(0, cp[i-1]-1),rep(i-1, cp[i]-cp[i-1]), rep(0, 136-cp[i]+1))
      }
    }
    
    delta[,k+1] <- c(rep(0,cp[k]-1), rep(k, 136-cp[k]+1))
    d <- as.matrix(rowSums(delta))
    
    fit1.ols <- lm(point ~ factor(Q) + time + factor(d))
    
    fit2.ols <- lm(point ~ factor(Q) + time)
    
    # Write changepoint locations to file (each will be a list)
    cat(c(lat,lon,index,cp),  file=file(sprintf("./cp_locations_%d.txt", Sys.getpid()), open = "a"), sep = " ", append = TRUE)
    cat("\n",  file=file(sprintf("./cp_locations_%d.txt", Sys.getpid()), open = "a"), append = TRUE)
    
  }else{ # No changepoints
    
    fit1.ols <- lm(point ~ factor(Q) + time) # models w/ and w/o changepoints will be same
    
    fit2.ols <- lm(point ~ factor(Q) + time)
    
  }
  
  s1 <- summary(fit1.ols)
  s2 <- summary(fit2.ols)
  
  write(as.vector(c(lat,lon,index,fit1.ols$coefficients[1],fit1.ols$coefficients[2],fit1.ols$coefficients[3],
                    fit1.ols$coefficients[4],fit1.ols$coefficients[5],coef(summary(fit1.ols))[1, "Std. Error"],
                    coef(summary(fit1.ols))[2, "Std. Error"],coef(summary(fit1.ols))[3, "Std. Error"],
                    coef(summary(fit1.ols))[4, "Std. Error"],coef(summary(fit1.ols))[5, "Std. Error"], numCP)),
        file=file(sprintf("./CP_output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 14, append=TRUE)
  
  write(as.vector(c(lat,lon,index,fit2.ols$coefficients[1],fit2.ols$coefficients[2],fit2.ols$coefficients[3],
                    fit2.ols$coefficients[4],fit2.ols$coefficients[5],coef(summary(fit2.ols))[1, "Std. Error"],
                    coef(summary(fit2.ols))[2, "Std. Error"],coef(summary(fit2.ols))[3, "Std. Error"],
                    coef(summary(fit2.ols))[4, "Std. Error"],coef(summary(fit2.ols))[5, "Std. Error"])),
        file=file(sprintf("./output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 13, append=TRUE)
}


registerDoParallel(cores=28)

sink("./sinked_output")

adply(global.index1, .margins = 1, .fun = trendAnalysis, .parallel = TRUE)

sink()


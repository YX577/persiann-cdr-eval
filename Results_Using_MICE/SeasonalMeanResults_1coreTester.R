setwd("D:/output/")

library("plyr")
library("foreach")
library("doParallel")
library("mice")

source("mice.impute.2l.norm2.R")

# Read in Seasonal Mean data
df <- read.table(file="SeasonalMean/seasonalMean.txt", header = TRUE)


sample.index <- read.table(file="Index/sampleIndex.txt", header = TRUE, col.names = TRUE)
sample.index <- as.matrix(sample.index)



# Variables for regression model

n <- 136
time <- (1:n)/40

Q1 = rep(c(1, 0, 0, 0), n / 4)
Q2 = rep(c(0, 1, 0, 0), n / 4)
Q3 = rep(c(0, 0, 1, 0), n / 4)
Q4 = rep(c(0, 0, 0, 1), n / 4)

One <- Q1+Q2+Q3+Q4
Q <- Q1 + 2*Q2 + 3*Q3 + 4*Q4


lonBound <- c(0.125, 359.875)
latBound <- c(59.375,-59.375)



# Function to retrieve seasonal data from data frame for point & surrounding points

getSeasData <- function(Y){
  y.lat <- Y[1]
  y.lon <- Y[2]
  seasData <- as.matrix(with(df, df[ Lat == y.lat & Lon == y.lon, ]))
  return(seasData)
}


simple.lm <- function(point,lat,lon,index){
  
  fit.ols <- lm(point ~ factor(Q) + time)
  
  s <- summary(fit.ols)
  
  write(as.vector(c(lat,lon,index,fit.ols$coefficients[1],fit.ols$coefficients[2],fit.ols$coefficients[3],
                    fit.ols$coefficients[4],fit.ols$coefficients[5],coef(summary(fit.ols))[1, "Std. Error"],
                    coef(summary(fit.ols))[2, "Std. Error"],coef(summary(fit.ols))[3, "Std. Error"],
                    coef(summary(fit.ols))[4, "Std. Error"],coef(summary(fit.ols))[5, "Std. Error"])),
        file=file(sprintf("ChangePoints/output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 13, append=TRUE)
}

# Function to (try to) impute missing values, then run linear regression and output coefficients 

impute.lm <- function(row.index){
  
  index <- as.numeric(row.index[1])
  lat <- as.numeric(row.index[2])
  lon <- as.numeric(row.index[3])
  
  if((lat %in% latBound)||(lon %in% lonBound)){
    
    Y <- cbind(lat,lon)
    point <- getSeasData(Y)
    point <- as.matrix(point[,c(-1,-2)])
    
    simple.lm(point,lat,lon,index)
    
  }else{
    
    Y = matrix(c(lat+.25,lon-.25,lat+.25,lon,lat+.25,lon+.25,lat,lon-.25,lat,lon,
                 lat,lon+.25,lat-.25,lon-.25,lat-.25,lon,lat-.25,lon+.25),
               nrow = 9, ncol = 2, byrow = TRUE)
    
    rownames(Y) <- paste("Y", c(1:9), sep="")
    
    point.df <- foreach(Y=iter(Y, by='row'),.combine=rbind) %do% getSeasData(Y)
    point.df <- t(point.df[,c(-1,-2)])
    
    colnames(point.df) <- paste("Y", c(1:9), sep="")
    point.df <- cbind(point.df, Q, time)
    
    
    tryCatch({
      
      ini <- mice(point.df, maxit = 0)
      pred <- ini$predictorMatrix
      
      pred[1:9,"Q"] <- -2
      
      tempData <- mice(point.df, method = c("2l.norm2","2l.norm2","2l.norm2",
                                            "2l.norm2","2l.norm2","2l.norm2",
                                            "2l.norm2","2l.norm2","2l.norm2","",""),
                       predictorMatrix = pred, m=15, seed=500)
      
      fit <- with(tempData,lm(Y5 ~ factor(Q) + time))
      
      p.fit <- summary(pool(fit))
      
      write(as.vector(c(lat,lon,index,p.fit[1,"est"],p.fit[2,"est"],p.fit[3,"est"],p.fit[4,"est"],p.fit[5,"est"],
                        p.fit[1,"se"],p.fit[2,"se"],p.fit[3,"se"],p.fit[4,"se"],p.fit[5,"se"])),
            file=file(sprintf("ChangePoints/output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 13, append=TRUE)
      
    }, error = function(err) {
      pt <- point.df[,"Y5"]
      
      simple.lm(pt,lat,lon,index)
      
    })
  }
}


adply(sample.index, .margins = 1, .fun = impute.lm, .parallel = FALSE)


library("mice")
library("plyr")
library("foreach")
library("doParallel")

source("./mice.impute.2l.norm2.R")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This R program reads PERSIANN-CDR seasonal mean precipitation data and             ]*#
#*[            estimates regression coefficients of the model below after imputing                ]*#
#*[            missing values by multiple imputation.                                             ]*#
#*[                                                                                               ]*#
#*[     Model:  Y = B1 + B2*Q2 + B3*Q3 + B4*Q4 + B5*time + e                                      ]*#
#*[                                                                                               ]*#
#*[             The coeffient of the time variable will be used to                                ]*#
#*[             visualize trends in seasonal mean precipitation.                                  ]*#
#*[                                                                                               ]*#
#*[  Output: Lat Lon Index Int.est Q2.est Q3.est Q4.est Time.est Int.se Q2.se Q3.se Q4.se Time.se ]*#
#*[          (13 columns for each grid point)                                                     ]*#
#*[-----------------------------------------------------------------------------------------------]*#

cores <- detectCores()
cluster <- makeCluster(cores) # Reserve one core for other use
registerDoParallel(cluster)

df <- read.table(file="~/data/seasonalMean.txt", header = TRUE)

global.index1 <- read.table(file="global.index1.txt", header = TRUE) # Global data broken into 6
global.index1 <- as.matrix(global.index)                             # files for faster processing

# Variables for regression model
n <- 136
time <- (1:n)/40
Q1 <- rep(c(1, 0, 0, 0), n / 4)
Q2 <- rep(c(0, 1, 0, 0), n / 4)
Q3 <- rep(c(0, 0, 1, 0), n / 4)
Q4 <- rep(c(0, 0, 0, 1), n / 4)
Q <- Q1 + 2*Q2 + 3*Q3 + 4*Q4
One <- Q1+Q2+Q3+Q4

# Will use simple regression for
# points on border
lonBound <- c(0.125, 359.875)
latBound <- c(59.375,-59.375)


# This function will retrieve seasonal mean data
# for target point & 8 surrounding points
# after called with foreach()
#  ___________
# |   |   |   |
# |___|___|___|
# |   | X |   |
# |___|___|___|
# |   |   |   |
# |___|___|___|
# Returns a 136 x 8 array
getSeasData <- function(Y){
  y.lat <- Y[1]
  y.lon <- Y[2]
  seasData <- as.matrix(with(df, df[ Lat == y.lat & Lon == y.lon, ]))
  return(seasData)
}

# This function computes and writes regression coefficients, 
# ignoring any missing values. It is used when multiple 
# imputation fails or cannot be done (border points).
simple.lm <- function(point,lat,lon,index){
  
  fit.ols <- lm(point ~ factor(Q) + time)
  
  s <- summary(fit.ols)
  
  write(as.vector(c(lat,lon,index,fit.ols$coefficients[1],fit.ols$coefficients[2],fit.ols$coefficients[3],
                    fit.ols$coefficients[4],fit.ols$coefficients[5],coef(summary(fit.ols))[1, "Std. Error"],
                    coef(summary(fit.ols))[2, "Std. Error"],coef(summary(fit.ols))[3, "Std. Error"],
                    coef(summary(fit.ols))[4, "Std. Error"],coef(summary(fit.ols))[5, "Std. Error"])),
        file=file(sprintf("./output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 13, append=TRUE)
}

# Function to impute missing values using mutliple imputation,
# then calculate regression coeffients of the imputed data, 
# then output results.
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
    
    tryCatch({ # mice will fail if seasonal median vector is all 0's
      
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
            file=file(sprintf("./output_%d.txt", Sys.getpid()), open = "a"),ncolumns = 13, append=TRUE)
      
    }, error = function(err) {
      pt <- point.df[,"Y5"]
      
      simple.lm(pt,lat,lon,index)
      
    })
  }
}


registerDoParallel(cores=28)

# sink the mice output
sink("./sinked_output")

adply(global.index1, .margins = 1, .fun = impute.lm, .parallel = TRUE)

sink()

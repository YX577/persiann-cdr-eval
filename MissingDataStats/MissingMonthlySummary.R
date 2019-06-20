library(matlab)
library(xtable)

setwd("D:/output/")

#*[--------------------------------------------------------------------]*#
#*[ This code produces summary statistics for monthly missing values.  ]*#
#*[--------------------------------------------------------------------]*#

year.first <- 1983
year.last <- 2015

dfList <- replicate(12, data.frame())
names(dfList) <- paste(month.abb[1:12])

for(i.year in year.first:year.last){
  
  print(i.year)
  
  df <- read.table(file=paste("missing_values/", i.year, "_Missing.txt", sep = ""), header = TRUE, nrows = 1000)

  for(i.month in 1:12){
    
        if(i.year == 1983){
    
          latlon <- df[,c(1,2)]
          dfList[[i.month]] <- latlon
        }
    
    dfList[[i.month]] <- cbind(dfList[[i.month]], df[,month.abb[i.month]])
    
  }
  
}

colnames <- c("lat", "lon", as.character(c(1983:2015)))
dfList <- lapply(dfList, setNames, colnames)

sapply(names(dfList), function (x) write.table(dfList[[x]], file=paste("D:/output/missing_monthly_head/", x, "_monthly_missing.txt", sep = ""), append = FALSE,  row.names = FALSE, col.names = TRUE))

dfSummaryList <- list()
dfQuantileList <- list()

dfSummaryList <- lapply(names(dfList), function (x) t(apply(dfList[[x]][,c(-1,-2)], 1, summary) )) 
dfQuantileList <- lapply(names(dfList), function (x) t(apply(dfList[[x]][,c(-1,-2)], 1, quantile, c(0.05, 0.95) )) )

dfListStats <- Map(cbind,dfSummaryList, dfQuantileList)
names(dfListStats) <- paste(month.abb[1:12])

sapply(names(dfListStats), function (x) write.table(dfListStats[[x]], file=paste("D:/output/missing_monthly_head/", x, "_monthly_summary.txt", sep = ""), append = FALSE,  row.names = FALSE, col.names = TRUE))

# View(dfListStats[[1]])

# sapply(names(dfListStats), function (x) View(dfListStats[[x]]))

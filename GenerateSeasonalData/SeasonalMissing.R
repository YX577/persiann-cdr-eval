setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This R program generates statistics for missing seasonal data which we             ]*#
#*[            generated from PERSIANN-CDR daily data.                                            ]*#
#*[            This will aid in the process of setting a tolerance for # of missing values for    ]*#
#*[            calculating sesonal mean, median and max data.                                     ]*#
#*[ Author: Maria Paquin                                                  __       __             ]*#
#*[ Updated: 04/6/2017                                                    ) \     / (             ]*#
#*[                                                                      )_  \_V_/  _(            ]*#
#*[                                                                        )__   __(              ]*#
#*[                                                                           '-'                 ]*#
#*[-----------------------------------------------------------------------------------------------]*#

year.first <- 1984
year.last <- 2015

dfList <- list()
i <- 1
# Read in 1983, output seasonal data, hold on to Dec

df <- read.table(file=paste("missing_values/1983_Missing.txt", sep = ""), header = TRUE, nrows = 100)
  
seasons <- df[,c(1,2)]

seasons$Winter <- rowSums(df[,c("Jan", "Feb")])
seasons$Spring <- rowSums(df[,c("Mar", "Apr", "May")])
seasons$Summer <- rowSums(df[,c("Jun", "Jul", "Aug")])
seasons$Fall <- rowSums(df[,c("Sep", "Oct", "Nov")])

write.table(seasons, file=paste("D:/output/seasonal_missing/1983_seasonal.txt", sep = ""),
            row.names = FALSE, col.names = TRUE)
dfList[[i]] <- seasons

endSeason <- df["Dec"]

# Read in the rest of the years, adding Dec from previous year to winter of curr year

for(i.year in year.first:year.last){
  
  i <- i + 1

  df <- read.table(file=paste("missing_values/", i.year, "_Missing.txt", sep = ""), header = TRUE, nrows = 100)
  
  endSeason <- cbind(endSeason,df[,c("Jan", "Feb")]) # Combine Dec of previous year to Winter of curr year
  endSeason$winter <- rowSums(endSeason) # sum Dec, Jan, Feb
  
  seasons <- df[,c(1,2)]
  
  seasons$Winter <- endSeason$winter
  seasons$Spring <- rowSums(df[,c("Mar", "Apr", "May")])
  seasons$Summer <- rowSums(df[,c("Jun", "Jul", "Aug")])
  seasons$Fall <- rowSums(df[,c("Sep", "Oct", "Nov")])
  
  write.table(seasons, file=paste("D:/output/seasonal_missing/", i.year, "_seasonal.txt", sep = ""),
              row.names = FALSE, col.names = TRUE)
  dfList[[i]] <- seasons
  
  endSeason <- df["Dec"]
 
}

names(dfList) <- paste0("Year", c(1983:2015))


dfList2 <- list()

dfList2[[1]] <- df[,c(1,2)]
dfList2[[2]] <- df[,c(1,2)]
dfList2[[3]] <- df[,c(1,2)]
dfList2[[4]] <- df[,c(1,2)]

for(i in seq_along(dfList)){
  
  dfList2[[1]] <- cbind(dfList2[[1]], dfList[[i]][3])
  dfList2[[2]] <- cbind(dfList2[[2]], dfList[[i]][4])
  dfList2[[3]] <- cbind(dfList2[[3]], dfList[[i]][5])
  dfList2[[4]] <- cbind(dfList2[[4]], dfList[[i]][6])

}

colnames <- c("lat", "lon", as.character(c(1983:2015)))
dfList2 <- lapply(dfList2, setNames, colnames)

names(dfList2) <- c("Winter", "Spring", "Summer", "Fall")

dfSummaryList <- list()
dfQuantileList <- list()

dfSummaryList <- lapply(names(dfList2), function (x) t(apply(dfList2[[x]][,c(-1,-2)], 1, summary))) 
dfQuantileList <- lapply(names(dfList2), function (x) t(apply(dfList2[[x]][,c(-1,-2)], 1, quantile, c(0.05, 0.95))))

dfListStats <- Map(cbind,dfSummaryList, dfQuantileList)
names(dfListStats) <- c("Winter", "Spring", "Summer", "Fall")

sapply(names(dfListStats), function (x) 
  write.table(dfListStats[[x]], 
              file=paste("D:/output/missing_seasonal_summary/", x, "_seasonal_summary.txt", sep = ""), 
              append = FALSE,  row.names = FALSE, col.names = TRUE))

View(dfListStats$Winter)
View(dfListStats$Spring)
View(dfListStats$Summer)
View(dfListStats$Fall)

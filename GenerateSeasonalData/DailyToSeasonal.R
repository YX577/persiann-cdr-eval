setwd("D:/output/")

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This R program generates seasonally formated data from PERSIANN-CDR daily data.    ]*#
#*[            The resulting datasets will be used in calculating seasonal statistics.            ]*#
#*[-----------------------------------------------------------------------------------------------]*#


year.first <- 1984
year.last <- 2016

i <- 1

JF <- c("Jan", "Feb")
MAM <- c("Mar", "Apr", "May")
JJA <- c("Jun", "Jul", "Aug")
SON <- c("Sep", "Oct", "Nov")

dfYear <- list()

# Generate 1983 seasonal data frames seperately
df <- read.table(file=paste("daily/Step1_DailyPPT_1983.txt", sep = ""), header = TRUE)

dfYear[[1]] <- data.frame()
dfYear[[2]] <- df[,grep(paste(MAM,collapse="|"), colnames(df))]
dfYear[[3]] <- df[,grep(paste(JJA,collapse="|"), colnames(df))]
dfYear[[4]] <- df[,grep(paste(SON,collapse="|"), colnames(df))]

names(dfYear) <- (c("Winter", "Spring", "Summer", "Fall"))

endSeason <- df[,grep("Dec", colnames(df))]

lapply(names(dfYear), function (x) 
  write.table(dfYear[[x]], file=paste("D:/output/seasonal/1983/",x ,".txt", sep = ""), 
              append = FALSE,  row.names = FALSE, col.names = TRUE))

for(i.year in year.first:year.last){
  
  # print(i.year)
  
  i <- i + 1
  
  df <- read.table(file=paste("daily/Step1_DailyPPT_", i.year, ".txt", sep = ""), header = TRUE)
  
  dfYear <- list()
  
  dfYear[[1]] <- cbind(endSeason, df[,grep(paste(JF,collapse="|"), colnames(df))])
  dfYear[[2]] <- df[,grep(paste(MAM,collapse="|"), colnames(df))]
  dfYear[[3]] <- df[,grep(paste(JJA,collapse="|"), colnames(df))]
  dfYear[[4]] <- df[,grep(paste(SON,collapse="|"), colnames(df))]
  
  names(dfYear) <- (c("Winter", "Spring", "Summer", "Fall"))
  
  if(i.year == 2016){
    dfYear[[4]] <- data.frame()
    }
  
  lapply(names(dfYear), function (x) 
    write.table(dfYear[[x]], file=paste("D:/output/seasonal/", i.year, "/", x ,".txt", sep = ""), 
                append = FALSE,  row.names = FALSE, col.names = TRUE))
  
  endSeason <- df[,grep("Dec", colnames(df))]
}

setwd("D:/output/")

#*[--------------------------------------------------------------------]*#
#*[ This code produces summary statistics for yearly missing values.   ]*#
#*[--------------------------------------------------------------------]*#

df <- read.table(file="missing_values/Yearly_Missing.txt", header = TRUE)

dfSummary<- t(apply(df[,c(-1,-2)], 1, summary))
dfQuantile <- t(apply(df[,c(-1,-2)], 1, quantile, c(0.05, 0.95) ))

dfStats <- cbind(dfSummary, dfQuantile)

write.table(dfStats, file="D:/output/missing_yearly/YearlyNA_summary.txt", append = FALSE,  row.names = FALSE, col.names = TRUE)

View(dfStats)

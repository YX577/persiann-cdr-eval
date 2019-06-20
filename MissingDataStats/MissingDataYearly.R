setwd("D:/output/")

#*[--------------------------------------------------------------------]*#
#*[ This code counts the total number of missing values for each year, ]*#
#*[ and outputs the information in a txt file.                         ]*#     
#*[--------------------------------------------------------------------]*#

year.first <- 1983
year.last <- 2016


for(i.year in year.first:year.last){
  
  df <- read.table(file=paste("daily/Step1_DailyPPT_", i.year, ".txt", sep = ""), header = TRUE)
  
  if(i.year == 1983){
    
    latlon <- df[,c(1,2)]
    N.Miss <- latlon
    
  }
    
   x <- df[,c(-1,-2)]
   
   colname <- paste("N.Miss_", i.year)
    
   N.Miss[colname] <- apply(x, 1, function(x) sum(is.na(x)))
  
}

write.table(N.Miss, file=paste("D:/output/missing_values/Yearly_Missing.txt", sep = ""), 
            append = FALSE, row.names = FALSE, col.names = TRUE)

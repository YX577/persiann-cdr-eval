setwd("D:/output/")

#*[--------------------------------------------------------------------]*#
#*[  This code counts the number of missing values each month, and     ]*#
#*[  outputs the total for each year (34 txt files, each one is one    ]*#
#*[  year of monthly missing count. )                                  ]*#
#*[--------------------------------------------------------------------]*#

year.first <- 1983
year.last <- 2015


for(i.year in year.first:year.last){

  df <- read.table(file=paste("daily/Step1_DailyPPT_", i.year, ".txt", sep = ""), header = TRUE)
  
  if(i.year == 1983){
    
    latlon <- df[,c(1,2)]
    N.Miss <- latlon
  }
  
   for(i.month in 1:12){
     
      x <- df[,grep(month.abb[i.month], colnames(df))]
       
      N.Miss[,month.abb[i.month]] <- apply(x, 1, function(x) sum(is.na(x)))
       
  }

  write.table(N.Miss, file=paste("D:/output/missing_values/", i.year, "_Missing.txt", sep = ""),
              row.names = FALSE, col.names = TRUE)


}

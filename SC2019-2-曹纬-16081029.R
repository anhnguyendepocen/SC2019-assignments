##1.Please make a function load.file() to read a .csv file and transform the first column (a character representing date and time) using as.POSIXlt into R time format.

library(dplyr)
load.file <- function(filename){
  file <- read.csv(filename,sep = ',',header = TRUE)
  nfile <- mutate(file , Year = as.POSIXlt(time)$year+1900)
  nfile
}
a <-load.file("C:/Users/25070/Desktop/统计计算/第二次作业/temp/Sydney.csv")
nrow(a)
##2.Then apply load.file() to each filename using lapply().
x <- list(Bris = "C:/Users/25070/Desktop/统计计算/第二次作业/temp/Brisbane.csv",Cai = "C:/Users/25070/Desktop/统计计算/第二次作业/temp/Cairns.csv"
          ,Mel = "C:/Users/25070/Desktop/统计计算/第二次作业/temp/Melbourne.csv",Syd = "C:/Users/25070/Desktop/统计计算/第二次作业/temp/Sydney.csv")
y <- lapply(x, load.file)
y
##3.How many rows of data are there for each city?
sapply(y,nrow)

##4.What is the hottest temperature recorded by city?
for (i in 1:4) {
  t <- max(y[[c(i,4)]])
  print(t)
}

##5.Estimate the autocorrelation function for each city.
c <- lapply(y, acf)
c

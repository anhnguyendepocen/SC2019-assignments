setwd("D:/ProgramProject/R/Lab3")

load.file <- function(file="Brisbane.csv"){
  f <- read.csv(file)
  f$time <- as.POSIXlt(f$time)
  f$temp.max
}
cityfile <- c("Brisbane.csv","Cairns.csv","Melbourne.csv","Sydney.csv")
city <-lapply(cityfile, load.file)
sapply(city,length)
sapply(city, max)
lapply(city, acf)

    
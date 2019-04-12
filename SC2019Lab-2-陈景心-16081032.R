load.file <- function(filename){
  temp <- read.csv(filename,header=TRUE,sep=",")
  temp[[1]] <- as.POSIXlt(temp[[1]])
  temp
}

x <- list("D:/BUAA/2019春/统计计算/temp/Brisbane.csv","D:/BUAA/2019春/统计计算/temp/Cairns.csv",
          "D:/BUAA/2019春/统计计算/temp/Melbourne.csv","D:/BUAA/2019春/统计计算/temp/Sydney.csv")

temp <- lapply(x,load.file)

lapply(temp,nrow) #每个城市数据行数

calmax <- function(filename){
  temp <- read.csv(filename,header=TRUE,sep=",")
  max(temp[[4]])
}
lapply(x, calmax) #各城市最高气温

acftemp <- function(filename){
  temp <- read.csv(filename,header=TRUE,sep=",")
  acf(temp[[2]])
}
lapply(x, acftemp) #估计各城市气温自相关函数
#定义load.file函数
load.file <- function(x,...){
  a <- read.csv(x,...)
  a[,1] <- as.POSIXct(a[,1]);a
}

#读入四个城市的数据，并将第一列转换为POSIXlt格式
loc_file <- list(Brisbane = "E:/temp/Brisbane.csv", Cairns = "E:/temp/Cairns.csv", Melbourne = "E:/temp/Melbourne.csv", Sydney = "E:/temp/Sydney.csv")
file <- lapply(loc_file, load.file);file

#计算每个城市数据的行数
lapply(file, nrow)

#查找每个城市记录的最高气温
sapply(file, function(x){
  max(x[,4])
})

#计算每个城市temp,temp.min,temp.max的自相关系数
lapply(file, function(x){
  for(i in 2:ncol(x))
    acf(x[i], lag = 100)
})
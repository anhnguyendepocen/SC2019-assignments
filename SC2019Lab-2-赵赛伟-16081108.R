####SC2019Lab-2-赵赛伟-16081108####
##1.创建load.file()函数（导入csv文件，将第一列变量转换为R软件的时间变量）
load.file<-function(file)
{
  data <- read.csv(paste(file))
  data[[1]] <- as.POSIXlt(data[[1]])
  data
}

##2.用lapply（）来应用1中的函数
library(dplyr)  #导入dplyr包以使用lapply函数
data.Brisbane <- lapply("C:/Users/Lenovo/Desktop/temp/Brisbane.csv", load.file)
data.Cairns <- lapply("C:/Users/Lenovo/Desktop/temp/Cairns.csv", load.file)
data.Melbourne <- lapply("C:/Users/Lenovo/Desktop/temp/Melbourne.csv", load.file)
data.Sydney <- lapply("C:/Users/Lenovo/Desktop/temp/Sydney.csv", load.file)

#减少代码重复，改用for循环进行
file.list <- list.files("C:/Users/Lenovo/Desktop/temp")
setwd("C:/Users/Lenovo/Desktop/temp")
datas <- lapply(file.list, load.file)


##3.行数量（创建行的计算函数）
rows.num <- function(data){
  length(data[1]$time)
}
rows_num <- sapply(datas,rows.num)
rows_num

##4.所有城市的最高温度
max.temp <- function(data){
  data.frame(data)
  max(data$temp.max)
}
#先求每个城市的最高温度，然后求城市最高温度的最大值
hottest_temp <- sapply(datas,max.temp)
max(hottest_temp)


##5.估算每个城市的自相关函数
acf.city<-function(data)
{
  data.frame(data)
  acflist<-lapply(data[,c(2:4)],acf)
  sapply(acflist,function(acf){acf$acf})
}
acf_city <- lapply(datas,acf.city)
acf_city


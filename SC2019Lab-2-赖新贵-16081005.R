
###lesson 3 control structures
setwd("E:/好好学习 天天向上/各科复习资料/统计计算/数据")
filename<-list(Melbourne="Melbourne.csv",Sydney="Sydney.csv",Brisbane="Brisbane.csv",Cairns="Cairns.csv")

##construct a function load.file()
load.file<-function(filename){
  filename<-read.csv(filename)
  as.POSIXlt(filename$time)
  return(filename)
}

##apply load.file() to each filename and assign the result to variable "temp"
temp<-lapply(filename,load.file)

##compute the numbers of rows for each city
nrows<-sapply(temp,nrow)

#the hottest temperature
max(sapply(temp, function(x){
  max(x[,4])
}))

##Estimate the autocorrelation function for each city.
#construct a function autocor to estimate the autocorrelation function
autocor<-function(x){
  data<-x[,2]               #获取温度数据
  data1<-data[1:(nrow(x)-1)]
  lagn<-data[2:nrow(x)]     #构造滞后一期的温度数据
  x.fit<-lm(lagn~data1)     #拟合线性函数
  summary(x.fit)
}
#apply autocor to each city
lapply(temp,autocor)

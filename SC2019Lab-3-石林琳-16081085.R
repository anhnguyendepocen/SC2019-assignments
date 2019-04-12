#(1)写一个可以读.csv文件并将第一列元素转化为R语言时间数据的函数load.file()
load.file<-function(x,...)
{
  x<-read.csv(x,...)
  y<-x$time
  lapply(y,as.POSIXlt)
}
x<-'C:/Users/Por/Desktop/R/Melbourne.csv'
load.file(x)

#(2)用lapply()将load.file()函数应用于每个文件
a<-'C:/Users/Por/Desktop/R/Sydney.csv'
b<-'C:/Users/Por/Desktop/R/Brisbane.csv'
c<-'C:/Users/Por/Desktop/R/Cairns.csv'
data<-list(x,a,b,c)
lapply(data,load.file)

#(3)求各个城市的数据行数
data<-lapply(data,read.csv)
lapply(data,nrow)

#(4)求每个城市记录的最高温度
max.file<-function(x)
{
  data.frame(x)
  max(x$temp.max)
}
lapply(data,max.file)

#(5)求每个城市的自相关系数
cor.file<-function(x)
{
  data.frame(x)
  x<-ts(x$temp)
  acf(x,lag=6)
}
lapply(data,cor.file)
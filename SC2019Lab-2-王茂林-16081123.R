setwd("C:\\Users\\hp\\Desktop\\大三下\\统计计算\\SC2019Lab-2-王茂林-16081123")
file_list<-list.files("temp\\temp")
setwd("C:\\Users\\hp\\Desktop\\大三下\\统计计算\\SC2019Lab-2-王茂林-16081123\\temp\\temp")
###############
#Lab Session 1#
###############
#read file and transfor the first column to type of POSIXlt
load.file<-function(file)
{
  f<-read.csv(paste(file))
  f[[1]]<-as.POSIXlt(f[[1]])
  f
}
#calculate rows of data
num.file<-function(d)
{
  length(d[1]$time)
}
#find the max of data
max.file<-function(d)
{
  data.frame(d)
  max(d$temp.max)
}
#calculate the autocorrelation of data
acf.file<-function(d)
{
  data.frame(d)
  acflist<-lapply(d[,c(2:4)],acf)
  sapply(acflist,function(acf){acf$acf})
}
library(dplyr)
#execute each function to each data
data <- lapply(file_list,load.file)
row <- sapply(data,num.file)
row
high <- sapply(data,max.file)
high
acfunction<-lapply(data,acf.file)
acfunction


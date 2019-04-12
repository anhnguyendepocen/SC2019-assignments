#读取文件
pools<-read.csv("D:/BUAA/2019春/统计计算/swimming_pools.csv",header=TRUE,sep=",")
#or pools<-read.table("D:/BUAA/2019春/统计计算/swimming_pools.csv",header=TRUE,sep=",")
#写入文件
write.table(pools,"D:/BUAA/2019春/统计计算/swimming_pools.txt",quote=FALSE,row.names=FALSE,sep="\t")
tpools<-read.table("D:/BUAA/2019春/统计计算/swimming_pools.txt",header=TRUE,sep="\t")
#or dput(pools,"D:/BUAA/2019春/统计计算/swimming_pools.R")
#rpools<-dget("D:/BUAA/2019春/统计计算/swimming_pools.R")
#or save(pools,file="D:/BUAA/2019春/统计计算/swimming_pools.RData")
#subset operations
pools[1]#提取数据框中的第一列
pools[c(1,3)]#提取数据框第一第三列
pools[[4]]#提取Longitude项的数据
pools$Longitude#提取Longitude项的数据

library(dplyr)
subset1<-filter(airquality,Temp>80 & Month>5)#返回温度大于80，五月后的所有行
airquality<-mutate(airquality,Celsius = (Temp-32)/1.8)#增加一列摄氏温度
months<-group_by(airquality,Month)#按月分组
summarise(months,meanTemp=mean(Temp))#计算每月平均温度
filter(airquality,Month!=5)%>%group_by(Month)%>%summarise(meanT=mean(Temp))
#删除五月份数据按月分组计算每月平均温度


#安装"dplyr"包及预处理
install.packages("dplyr")
library(dplyr)

data(package='datasets')#获取datasets中所有数据集
data("airquality")#读入airquality数据集

dim(airquality)#查看数据
str(airquality)#查看变量组成
head(airquality)#查看部分读到的数据

#(1)提取Temp>80且在5月份后的所有行
air.f<-filter(airquality,Temp>80 & Month>5)
str(air.f)

#(2)添加一个以摄氏度显示温度的新列
airquality<-mutate(airquality,Celsius=round((Temp-32)/1.8,1))
head(airquality)

#(3)计算每个月的平均温度
Months<-group_by(airquality,Month)
summarize(Months,Tempmean=mean(Temp,na.rm=TRUE))

#(4)删除所有与Month=5对应的数据，按月分组，求出每个月的平均温度
filter(airquality,Month!=5) %>%
  group_by(Month) %>%
  summarize(Tempmean=mean(Temp,na.rm=TRUE))

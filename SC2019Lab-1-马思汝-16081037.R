#LESSON_1
#1.Try read.csv() and read.table() to import "swimming_pools.csv" as a data frame with the name pools.
pools <- read.csv("C:/Users/THINK/Desktop/swimming_pools.csv", header = TRUE)

#2.Try write.table(), dput(), and save() functions to write pools to files.
write.table (pools, file ="C:/Users/THINK/Desktop/pools1.txt", sep =" ", row.names =FALSE, col.names =TRUE, quote =TRUE)
#输出数据文档，以空格为分割列，导出列名，字符串用引号表示
dput(pools, file = "C:/Users/THINK/Desktop/pools2.txt")
#采用dput函数输出数据文件
save(pools, file = "C:/Users/THINK/Desktop/pools3.Rdata")
#采用save函数输出数据文件

#3.Restart R and read your saved data in R.
pools1 <- read.table("C:/Users/THINK/Desktop/pools1.txt", sep =" ", header = TRUE)
#对应读入write.table写出文件
pools2 <- dget("C:/Users/THINK/Desktop/pools2.txt")
#对应读入dput写出文件
pools3 <- load('C:/Users/THINK/Desktop/pools3.Rdata')
#对应读入save写出文件

#4.Practice subsetting of a data frame.
pools[c(1,3)]#读取数据框中的第一列和第三列
pools[2]#读取数据框中的第二列，返回类型依旧为数据框
pools[[2]]#读取数据框中的第二类，提取单个元素
pools$Longitude#读取数据框中的Longitude列（按照数据框中列名提取）




#LESSON_2_You'll be working with the airquality in the R package datasets. Bear in mind %>%.
head(airquality)#观察数据
library(dplyr)#加载dplyr程序包

#1.Please return all the rows where Temp is larger than 80 and Month is after May.
air1 <- filter(airquality, Temp > 80 & Month >5)#筛选温度大于80的六月起的信息子集

#2.Please add a new column that displays the temperature in Celsius.
airquality <- mutate(airquality, Celtemp = (Temp - 32) / 1.8)#根据华氏温度新增摄氏温度

#3.Calculate the mean temperature in each month.
air3 <- group_by(airquality, Month)#按照月份分组
summarise(air3, mtemp = mean(Temp, na.rm = TRUE))#计算每月的平均华氏温度

#4.Remove all the data corresponding to Month = 5, group the data by month, and then find the mean of the temperature each month.
filter(airquality, airquality$Month > 5) %>% group_by(Month) %>% summarise(meantemp = mean(Temp, na.rm = TRUE))
#删除5月的数据后计算月平均温度
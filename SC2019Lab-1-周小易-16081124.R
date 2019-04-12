####统计计算第一次作业--16081124--周小易
###Lab Session 1
##You'll be working with swimming_pools.csv; it contains data on swimming poolsin Brisbane,
#Australia (Source: data.gov.au).
#The file contains the column namesin the first row.
#It uses a comma to separate values within rows.

#1. Try read.csv() and read.table() to import "swimming_pools.csv" as a dataframe with the name pools.
pools<-read.csv("swimming_pools.csv")
pools<-read.table("swimming_pools.csv",sep = ",",header = T)#需要写分隔符

#2. Try write.table(), dput(), and save() functions to write pools to files.
write.table(x = pools, file = "pools001.csv", sep = ",",row.names = F)#不含行名

dput(pools, file = "pools002.R")

save(pools,file = "pools003.RData")

list.files()#查看文件

#3. Restart R and read your saved data in R.
read.csv("pools001.csv")
dget("pools002.R")
load("pools003.RData")

#4. Practice subsetting of a data frame.
pools[2] #取第二列
pools[2,] #取第二行
pools[2,3]#取第二行第三列的数据
pools[-3]#去掉第三列
pools[3:10,1]#取3-10行、第一列的数据


##You'll be working with the airquality in the R package datasets. Bear in mind%>%.
library(dplyr)
#1. Please return all the rows where Temp is larger than 80 and Month is after May.
data("airquality")
airquality
subset1<-filter(airquality, Temp > 80 & Month > 5)

#2. Please add a new column that displays the temperature in Celsius.
airquality <- mutate(airquality, tempinCelsius = (Temp-32)/ 1.8)

#3. Calculate the mean temperature in each month.
summarise(airquality,tmean=mean(Temp,na.rm = T))#不考虑NA项

#4. Remove all the data corresponding to Month = 5,group the data by month,
#and then find the mean of the temperature each month.
filter(airquality,Month!=5) %>% group_by(Month) %>% summarise(tmeanbymonth=mean(Temp))





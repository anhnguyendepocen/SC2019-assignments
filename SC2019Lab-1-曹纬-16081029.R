#1.1
pools <- read.csv("C:/Users/25070/Desktop/统计计算/第一次作业/swimming_pools.csv",sep = ',',header = T)
head(pools)
#1.2
write.table(pools,"C:/Users/25070/Desktop/统计计算/pools1.txt",sep = ',')
dput(pools,file = "C:/Users/25070/Desktop/统计计算/pools2.txt")
save(pools,file = "C:/Users/25070/Desktop/统计计算/pools3.RData")
#1.3
load("C:/Users/25070/Desktop/统计计算/pools3.RData")
swim<-dget("C:/Users/25070/Desktop/统计计算/pools2.txt")
head(swim)
#1.4
a <- head(pools[c(1,4)]);a ##取出数据框pools的第一列与第四列数据内容（Name、Longitude）
head(pools$Name)
tail(pools[1])##取出数据框pools的第一列Name(前6项和后6项)


#2.1
library(dplyr)
air1 <- filter(airquality,Temp >80 & Month >5)
head(air1)
#2.2
air2 <- mutate(airquality,Celsius = (Temp-32)/1.8)
head(air2)
#2.3
mon <- group_by(airquality,Month)
summarize(mon,meanTemp = mean(Temp,na.rm = TRUE))
#2.4
filter(airquality,Month!=5)%>%group_by(Month)%>%summarize(meanTemp = mean(Temp,na.rm = TRUE))

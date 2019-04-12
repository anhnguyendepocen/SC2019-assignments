setwd("C:\\Users\\hp\\Desktop\\大三下\\统计计算")
###############
#Lab Session 1#
###############
pools <- read.csv("swimming_pools.csv")
pools
pools <- read.table("swimming_pools.csv",header = TRUE,sep = ",")
pools
write.table(pools,"pool.csv")
write.csv(pools,file = "pools.csv")
dput(pools,"pools1.RData")
save(pools,file = "pools2.RData")
#reopen R and clear the object from the workspace
re_pool <- read.table("pool.csv")
re_pools <- read.csv("pools.csv")
re_pools1 <- dget("pools1.RData")
load("pools2.RData")
#find the function "write.csv" will write the serial number of data
write.csv(pools,file = "pools3.csv",row.names = F)
re_pools3 <- read.csv("pools3.csv")
#practice submitting of a data frame
a<-pools[1]
a
b<-pools[[1]]
b
c<-pools[c(1,3)]
c
d<-pools[[c(1,3)]]#What is it?
d
#result [1] 3
pools[[1]][[3]]#choose the third sample in Name
pools[[c(2,3)]]
pools[[c(3,3)]]#choose the third sample in Latitude
pools[[c(4,3)]]
e<-pools["Name"]
e
f<-pools$Name
f
###############
#Lab Session 2#
###############
library(dplyr)
aq <- datasets::airquality
filter(aq,Temp>80 & Month > 5)#Q1
mutate(aq, Celsius = (Temp-32)/1.8)%>%
  group_by(Month)%>%
  summarise(Temp = mean(Temp,na.rm = T),Celsius = mean(Celsius,na.rm = T))#Q2Q3
mutate(aq, Celsius = (Temp-32)/1.8)%>%
  filter(Month>5|Month<5)%>%
  group_by(Month)%>%
  summarise(Temp = mean(Temp,na.rm = T),Celsius = mean(Celsius,na.rm = T))#Q2Q4
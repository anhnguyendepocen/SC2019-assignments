#L1
pools<- read.csv('F:/swimming_pools.csv',header = T)
pools<-read.table('F:/swimming_pools.csv',sep=",",header = T)
write.table(pools,file = "pool.csv", sep = ",")#csv
dput(pools,file = "pool.R")#R
dget(Pools,file = "pool.R")
save(pools,file = "pool.Rdata")#Rdata
load(Pools,file = "pool.Rdata")
pools[1:3]
pools[pools>0]
class(pools[[1]])#factor
class(pools[1])#data.frame
class(pools[[c(1,3)]])#integer
#L2
library(datasets)
library(dplyr)
str(airquality)
filter(airquality, Temp > 80,Month > 5)
mutate(airquality,CT = 5*(Temp-32)/9)%>%group_by(Month)%>%summarize(Temp = mean(Temp, na.rm = TRUE))
# A tibble: 5 x 2
#Month  Temp
#<int> <dbl>
#  1     5  65.5
#2     6  79.1
#3     7  83.9
#4     8  84.0
#5     9  76.9
filter(airquality,Month > 5)%>%group_by(Month)%>%summarize(Temp = mean(Temp, na.rm = TRUE))
# A tibble: 4 x 2
#Month  Temp
#<int> <dbl>
#  1     6  79.1
#2     7  83.9
#3     8  84.0
#4     9  76.9

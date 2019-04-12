###Lab session 1
##name:Su Feihu   StudentID:16081041

##Read and Write Data in R
#Import "swimming_pools.csv" as a data frame with the name pools.
pools <- read.csv("D:/study/dateset/statistical computing/swimming_pools.csv", header = TRUE)
head(pools)
str(pools)
pools <- read.table("D:/study/dateset/statistical computing/swimming_pools.csv", header = T, sep = ",")
#Write pools to files
write.table(pools, file = "D:/study/dateset/statistical computing/swimmingpools1.csv", sep = ",", col.names = TRUE)
pools1 <- read.csv("D:/study/dateset/statistical computing/swimmingpools1.csv", header = TRUE)

dput(pools, file = "D:/study/dateset/statistical computing/swimmingpools2.csv")
pools2 <- dget("D:/study/dateset/statistical computing/swimmingpools2.csv")

save(pools, file = "D:/study/RDate/swimmingpool.RDate")

#Restart R and read your saved data in R.
load("D:/study/RDate/swimmingpool.RDate")
ls()

#Practice subsetting of a data frame.
  #subset every pools' latitude.
pools[c(1,3)]
  #subset all longitude
pools[[4]]
  #get Centenary Pool (inner City) 's address
pools[[2]][[4]]


###Lab Session 1 Cont'd
head(airquality)
airquality
library(dplyr)
##return all the rows where Temp is larger than 80 and Month is after May.
(airquality_subset1 <- filter(airquality, Temp > 80 & Month > 5))
##add a new column that displays the temperature in Celsius.
airquality$Celsius <- round((airquality$Temp-32)/1.8, digits = 1)
head(airquality)
##Calculate the mean temperature in each month.
group_by(airquality, Month) %>%
 summarize(avgtemp = mean(Temp, na.rm = TRUE)) 
##Remove all the data corresponding to Month = 5,group the data by month, 
##and then find the mean of the temperature each month.
filter(airquality, Month != 5) %>%
  group_by(Month) %>%
  summarize(avgtemp = mean(Temp, na.rm = TRUE)) 

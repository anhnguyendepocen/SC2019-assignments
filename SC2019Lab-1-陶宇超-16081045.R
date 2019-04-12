#Statistical Computing Homework1 
#author:Yuchao Tao  StudentID:16081045

#read the swimming_pools data (two methods: read.csv / read.table)
pools <- read.csv("/Users/tyc_219/Desktop/swimming_pools.csv", header = TRUE)
pools
pools1 <- read.table("/Users/tyc_219/Desktop/swimming_pools.csv", header = T, sep = ",")
pools1
#use write.table(), dput(), and save() functions to write pools to files
write.table(pools, file = "swimmingpool.csv", sep = ",", col.names = TRUE)
read.csv("swimmingpool.csv", header = TRUE)
dput(pools, file = "swimpool.csv")
pools2 <- dget("swimpool.csv")
pools2
save(pools, file = "pool.RData")
ls()
rm(pools)
ls()
load("pool.RData")
ls()
#select all those pools latitude and longitude
pools[c(3,4)]
#get the swimming pool's localtion message which called "Fortitude Valley Pool" 
subset(pools, Name == "Fortitude Valley Pool", select = c(Latitude, Longitude))

library(datasets)
head(airquality)
#return all the rows where Temp is larger than 80 and Month is after May
air_hot <- filter(airquality, Temp > 80 & Month >5)
str(air_hot)
#add a new column that displays the temperature in Celsius
airquality$Celsius <- round((airquality$Temp-32)/1.8, digits = 1)
head(airquality)
#calculate the mean temperature in each month
avg_temp <- vector(mode = "numeric", length = 0)
for(i in 1:5){
  a <- subset(airquality, Month == (i+4), select = c(Temp)) %>% nrow
  b <- subset(airquality, Month == (i+4), select = c(Temp)) %>% sum
  avg_temp[i] <- round((b/a), digits = 1)
}
avg_temp
#remove all the data corresponding to Month = 5, group the data by month, and then find the mean of the temperature each month
airquality1 <- airquality %>% filter(Month != 5) %>% group_by(Month)
avg_temp1 <- avg_temp[2:5]
avg_temp1









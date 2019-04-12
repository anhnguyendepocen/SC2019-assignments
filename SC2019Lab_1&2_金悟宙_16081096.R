#LAB1
#Read data
dataswim1 <- read.csv("C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools.csv", header = T, sep =",")
dataswim2 <- read.table("C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools.csv", header = T, sep =",", quote = "\"")
dataswim <- dataswim1

#Write data
write.table(dataswim, file = "C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools_temp1.csv")
dput(dataswim, "C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools2.R")
save(dataswim, file = "C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools3.RData")

#Restart R and read dataframe:pools
load("C:/Users/Ozu/Desktop/学习资源/统计计算/swimming_pools3.RData")

#Subset of dateframe
subset_dataswim <- subset(dataswim, Latitude > -27.5 & Longitude > 152 )
subset_dataswim





#LAB2
library(dplyr)
head(airquality)

#Return all the rows where Temp is larger than 80 and Month is after May
result1 <- filter(airquality, Temp > 80 & Month > 5)
result1

#Add a new column that displays the temperature in Celsius
airquality <- mutate(airquality, Celsius = (Temp - 32)*5/9)
airquality

#Calculate the mean temperature in each month
result2 <- group_by(airquality, Month) %>% 
  summarise(Temperature = mean(Celsius, rm.na = T))
result2

#Remove all the data corresponding to Month = 5, group the data by month, and then find the mean of the temperature each month.
result3 <- filter(airquality, Month != 5) %>%
  group_by(Month) %>%
  summarise(Temperature = mean(Celsius, rm.na = T))
result3


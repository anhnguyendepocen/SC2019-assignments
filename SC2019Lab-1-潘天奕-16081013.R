####Lab1.1 Read and write data####
#read data
pools <- read.table('D:/statistic computing/Data/swimming_pools.csv',  header = T, sep = ",", quote = "\"")
  #ignore the inside comma with quote = "\""
pools <- read.csv('D:/statistic computing/Data/swimming_pools.csv')

#write data
write.table(pools, 'D:/statistic computing/Data/pools.csv')
dput(pools, 'D:/statistic computing/Data/pools.R')
save(pools, file = 'D:/statistic computing/Data/pools.RData')

#Restart R and read dataframe:pools
pools <- dget('D:/statistic computing/Data/pools.R')
load('D:/statistic computing/Data/pools.RData')

#subset of dateframe
str(pools)
head(pools)
summary(pools)
subset.pools <- subset(pools, Latitude > -27.5 & Longitude > 152 )
subset.pools

####Lab1.2 dplyr######
library(dplyr)
head(airquality)
str(airquality)

#return all the rows where Temp is larger than 80 and Month is after May
answer1 <- filter(airquality, Temp > 80 & Month > 5)
str(answer1)

#add a new column that displays the temperature in Celsius
airquality <- mutate(airquality, Celsius = (Temp - 32)/1.8)
head(airquality)

#Calculate the mean temperature in each month
group_by(airquality, Month) %>% 
  summarise(Temperature = mean(Celsius, rm.na = T))

#   Remove all the data corresponding to Month = 5, group the data by month, 
# and then find the mean of the temperature each month.
filter(airquality, Month != 5) %>%
  group_by(Month) %>%
  summarise(Temperature = mean(Celsius, rm.na = T))

#import swimming pools data
pools <- read.csv('Lab1/swimming_pools.csv')
pools <- read.table('Lab1/swimming_pools.csv', header = TRUE, sep = ',')

# write pools object to a file
write.table(pools, file = "Lab1/pools.txt", sep = ",")
dput(pools, file = 'Lab1/pools_dput.txt')
save(pools, file = 'Lab1/pools.Rdata')

# read saved pools data
load('Lab1/pools.Rdata')

# Practice subsetting of a data frame.
attach(pools)
pools$Name
pools[Latitude > -27.5,]

# dplyr practice
library(dplyr)
data("airquality")

## practice 1
filter(airquality, Temp > 80 & Month > 5)

## practice 2 add Celsius temperature
airquality <- mutate(airquality, celtemp = (Temp-32)/1.8)

## practice 3 calculate mean temparature
airquality %>%
  group_by(Month) %>%
  summarise(mean_temp = mean(Temp))

## practice 4 
airquality %>%
  filter(Month!=5) %>%
  group_by(Month) %>%
  summarise(mean_temp = mean(Temp))



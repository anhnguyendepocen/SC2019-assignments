########
#  L1  #
########
pools <- read.csv("C:/Users/hasee/Desktop/swimming_pools.csv")
POOLS <- read.table("C:/Users/hasee/Desktop/swimming_pools.csv",sep = ",")

write.table(pools,"C:/Users/hasee/Desktop/pools.csv",sep = ",")
read.csv("C:/Users/hasee/Desktop/pools.csv")

dput(pools,"C:/Users/hasee/Desktop/pools.R")
dget("C:/Users/hasee/Desktop/pools.R")

save(pools,file = "C:/Users/hasee/Desktop/pools.RData")
test<-load(file = "C:/Users/hasee/Desktop/pools.RData")
test
pools

pools
pools[1]
pools[1:2]
pools[[c(1,3)]]
pools[[1]][[3]]

########
#  L2  #
########

library(dplyr)
airquality

#1.#
filter(airquality,Month>5 & Temp > 60)
#2.#
airquality <- mutate(airquality, Ctemp = (Temp-32)/1.8)
airquality
#3.#
group_by(airquality,Month) %>% summarise(meantemp = mean(Temp))
#4.#
filter(airquality,Month!=5) %>% group_by(airquality,Month) %>% summarise(meantemp = mean(Temp))
#This Line doesn't work and I don't know how to fix it so I try another way.

newair <- filter(airquality,Month!=5) 
group_by(newair,Month) %>% summarise(meantemp = mean(Temp))

########
#  L3  #
########

#1.#
load.file <- function(x,...){
  templist <- read.csv(x,...)
  templist[[1]] <- as.POSIXct(templist[[1]])
  templist
}
Bribane <- load.file("C:/Users/hasee/Desktop/Brisbane.csv")
Bribane[[1]]

#2.#
loadlist <- c("C:/Users/hasee/Desktop/Brisbane.csv","C:/Users/hasee/Desktop/Cairns.csv",
              "C:/Users/hasee/Desktop/Melbourne.csv","C:/Users/hasee/Desktop/Sydney.csv")
lapply(loadlist, load.file)

#3.#
loadfileanddim <- function(x,...){
  templist <- read.csv(x,...)
  templist[[1]] <- as.POSIXct(templist[[1]])
  result <- dim(templist)
  result
}
lapply(loadlist, loadfileanddim)
#A little change to the the function made in 1 and use it,the rusult is 99,80,97,99

#4.#
loadfileandsummary <- function(x,...){
  templist <- read.csv(x,...)
  templist[[1]] <- as.POSIXct(templist[[1]])
  summary(templist)
}
lapply(loadlist, loadfileandsummary)
#The result is 23.89,35,13.33,23.33

#5.#
loadfileandacf <- function(x,...){
  templist <- read.csv(x,...)
  templist[[1]] <- as.POSIXct(templist[[1]])
  acf(templist)
}
lapply(loadlist, loadfileandacf)

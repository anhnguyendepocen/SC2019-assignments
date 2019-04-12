setwd("D:/ProgramProject/R/Lab1")
#read.csv
pools <- read.csv("swimming_pools.csv")
pools
#read.table
pools <- read.table("swimming_pools.csv",header = TRUE,sep = ",")
pools

#write.table
write.table(pools,file = "pools_wrtietable.csv",sep = ",",col.names = TRUE,row.names = FALSE)
pools_writetable <- read.csv("pools_wrtietable.csv")
pools_writetable
#dput
dput(pools,file = "pools_dput.csv")
dget("pools_dput.csv")
#save
save(pools,pools_writetable,file="pools.RData")

#close&resart

#load
rm(list = ls())
load(file = "pools.RData")

#subsetting
pools[1]
pools[[1]]
pools$Name


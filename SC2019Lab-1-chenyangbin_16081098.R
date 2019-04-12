#Try read.csv() and read.table() to import "swimming_pools.csv" as 
#a data frame with the name pools
pools1<-read.csv("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/swimming_pools.csv",sep = ",",header = T)
pools
pools2<-read.table("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/swimming_pools.csv",sep = ",",header = T)
pools2

#Try write.table(), dput(), and save() functions to write pools to files
write.table(pools,file="C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools1.csv")
dput(pools,file="C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools2.csv")
save(pools,file="C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools3.csv")

#Restart R and read your saved data in R.
read.csv("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools1.csv")
read.table("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools1.csv")

read.csv("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools2.csv")
read.table("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools2.csv")

read.csv("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools3.csv")
read.table("C:/Users/cyb/Desktop/学习文件/任务/统计计算/作业/1/pools3.csv")

#Practice subsetting of a data frame
name<-pools$Name
name
address<-pools$Address
address
latitude<-pools$Latitude
latitude
longitude<-pools$Longitude
longitude

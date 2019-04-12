###SC2019Lab-1-朱帅东-16081133
##Part 1
#Use read.csv 
pools <- read.csv("G:/大三下学期/统计计算/作业1/swimming_pools.csv",header = TRUE,sep = ",")
attributes(pools)
#Use read.table
pools <- read.table("G:/大三下学期/统计计算/作业1/swimming_pools.csv",sep = ",",header = TRUE)
attributes(pools)

#write pools to files
write.table(pools,file = "G:/大三下学期/统计计算/作业1/sps1.csv",sep = ",")
dput(pools,file = "G:/大三下学期/统计计算/作业1/sps2.csv")
save(pools,file = "G:/大三下学期/统计计算/作业1/sps3.csv")

#read saved data
pools2 <- read.table("G:/大三下学期/统计计算/作业1/sps1.csv",sep = ",",header = TRUE)
pools3 <- dget("G:/大三下学期/统计计算/作业1/sps2.csv")

#Practice subsetting of a data frame
#Use []
pools[1]
pools[1:4]
pools[c(1,2)]
pools[pools > 2]
pools[2,1]
pools[2,]
pools[,3]
#Use [[]]
pools[[1]]
pools[[c(4,2)]]
pools[[1]][[3]]
#Use $
pools$Latitude
pools[["Latitude"]]

subset(pools,select = Latitude:Longitude)
##Part 2
#生成5月之后，华氏度大于80的所有行
subset1 <- filter(airquality,Temp > 80 & Month > 5)

#计算并增加摄氏度变量
airquality1 <- mutate(airquality, Cel = round((Temp - 32)/1.8))
head(airquality1)

#计算每个月的平均气温
group_by(airquality1 , Month) %>%
  summarise(meanF = mean(Temp),meanC = mean(Cel))

#删去5月份的数据后计算每个月的平均气温
filter(airquality1 , Month != 5) %>%
  group_by(Month) %>%
  summarise(meanF = mean(Temp),meanC = mean(Cel))

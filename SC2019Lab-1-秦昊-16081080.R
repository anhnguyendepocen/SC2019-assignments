#Lab1
pools1 <- read.csv(file = "/Users/qinhao/Documents/swimming_pools.csv", 
                   sep=",", header = T, encoding = "UTF-8")
head(pools1)
pools2 <- read.table(file = "/Users/qinhao/Documents/swimming_pools.csv", 
                     sep=',', header = T)
head(pools2)

#使用三种方法写入数据框pools，可以看出存入文件的格式有所变化
write.table(pools1, file = "/Users/qinhao/Documents/sw_pool.csv")
save(pools1, file = "/Users/qinhao/Documents/pools1.RData")
dput(pools1, file = "/Users/qinhao/Documents/sw_pools1.csv")
#dput会将对象转化成文本数据

#再次读取存入的数据
p1 <- read.csv(file = "/Users/qinhao/Documents/sw_pool.csv", 
               header = T, sep = ",")
head(p1)
p_1 <- read.table(file = "/Users/qinhao/Documents/sw_pool.csv", 
                header = T, sep = ",")
head(p_1)

rm(pools1)
load("/Users/qinhao/Documents/pools1.RData")
head(pools1)

p3 <- dget(file = "/Users/qinhao/Documents/sw_pools1.csv")
head(p3)
#因为文件存入后格式的问题，读取save和dput存入的数据时，采用不同的方法(load,attach,dget)读取）

#对pools1数据框选取子集
pools1[[2]]
pools1$Address
#选取pools的地址数据
pools1[[2]][1:4]
#选取地址里面第一个到第四个
pools1[c(2,4)]
#选取pools中第二和第四个列表
pools1[[c(4,2)]]
#选取列表中的内嵌元素


#Lab1.2
library(dplyr)
#简要了解数据框airquality的结构
str(airquality)
head(airquality)

airquality <- rename(airquality, sun_radi = Solar.R)
#首先选出气温高于80且五月之后的所有行
air_data1 <- filter(airquality, Temp > 80 & Month>5)
air_data1

#增加一列摄氏温度的列
airquality <- mutate(airquality, Celtemp = 5*(airquality$Temp - 32)/9.0)
options(digits = 2)
head(select(airquality, Temp, Celtemp),5)

#计算每个月的平均气温
airquality <- arrange(airquality, Month)
month <- group_by(airquality, Month)
summarise(month, mean(Temp, na.rm = T))

#去除所有月份为5的数据，按月对数据分组，然后找出每个月的平均气温
filter(airquality, Month>5)%>%
  group_by(Month)%>%
  summarise(mean(Temp, na.rm = T))






















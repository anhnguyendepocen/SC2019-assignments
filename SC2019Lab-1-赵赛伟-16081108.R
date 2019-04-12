####SC2019Lab-1####
###R basics###
##1.导入数据
pools1 <- read.csv(file = "C:/Users/Lenovo/Desktop/swimming_pools.csv", sep = ",", header = TRUE)
pools2 <- read.table(file = "C:/Users/Lenovo/Desktop/swimming_pools.csv", sep = ",", header = TRUE)
pools <- read.table(file = "C:/Users/Lenovo/Desktop/swimming_pools.csv", sep = ",", header = TRUE)
#导入数据最好是格式相对应的，但是如果不对应，R软件也可以在一定程度上处理

##2.输出数据
write.table(pools, file = "C:/Users/Lenovo/Desktop/pools1.txt", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE)
write.csv(pools, file = "C:/Users/Lenovo/Desktop/pools1.csv", row.names = FALSE)
dput(pools, file = "C:/Users/Lenovo/Desktop/pools2.txt")  
save(pools, file = "C:/Users/Lenovo/Desktop/pools3.RDate")
#输出数据的时候可以看出，R软件对文件的格式要求比较严格（可以说R软件的处理比较呆板）

##3.重启并读取数据
epools1 <- read.table(file = "C:/Users/Lenovo/Desktop/pools1.txt", sep = ",", header = T)
epools2 <- read.csv("C:/Users/Lenovo/Desktop/pools1.csv",sep = ",",header = T)
epools3 <- dget("C:/Users/Lenovo/Desktop/pools2.txt")
eloops4 <- load("C:/Users/Lenovo/Desktop/pools3.RDate")
#重启后读取数据依旧可行，因为文件已经被储存在了本地

##4.数据框处理
#创建数据框
student <- data.frame(id = c(1,2,3,4,5,6,7,8,9,10), 
                      name = c("a","b","c",NA,"e","f","g","h","i","j"),
                      gender = c(NA,3,3,4,2,2,3,3,1,4),
                      year = c(18,17,18,17,18,18,18,18,19,NA))
#我们为了练习数据框操作，创建了一个学生信息数据框，包含学生id，姓名，年级，年龄四个变量（为了后续处理加入了一定的残缺值）

#数据框数据读取
student[1,]  #读取第一行的数据（即第一位学生信息）
student[,3]   #读取第三列的数据（即学生年级变量）
name <- student$name
name    #读取其中name变量
#去除残缺值行
good <- complete.cases(student)
head(student[good, ])


###dplyr包的安装和使用###
##dplyr包的安装（略）
##dplyr包的读入
library(dplyr)

##导入数据
library(datasets)
data(airquality) #获得datasets包中的airquality数据
summary(airquality)  #观察数据信息（为节省空间没有用View函数）

##1.选择Temp大于80切Month大于5
newdata1 <- filter(airquality, Temp > 80 & Month > 5)
summary(newdata1)  #观察数据信息

##2.增加新变量——摄氏度（用公式C = （F-32）/1.8）
newdata2 <- mutate(airquality, Tempc = (Temp - 32) / 1.8)
summary(newdata2)  #同上

##3.计算每月的平均气温
newdata3 <- mutate(airquality, month = Month)
months <- group_by(newdata3, month)
summarize(months, Temp = mean(Temp, na.rm = TRUE))
#结果是华氏度平均温度，若用摄氏度如下
newdata2_1 <- mutate(newdata2, month = Month)
months_1 <- group_by(newdata2_1, month)
summarize(months_1, Tempc = mean(Tempc, na.rm = TRUE))

##4.删除5月份数据，对月份数据分组并计算每月平均气温（这里仅以华氏度温度计算）
qq <- quantile(airquality$Month, seq(0,1,0.25), na.rm = TRUE) #对月份进行分组
newdata4 <- filter(airquality, Month > 5)
mutate(newdata4, month.group = cut(Month, qq)) %>%  #用管道进行连接
  group_by(month.group) %>% 
  summarize(Temp = mean(Temp, na.rm = TRUE)) #计算每组的温度平均值


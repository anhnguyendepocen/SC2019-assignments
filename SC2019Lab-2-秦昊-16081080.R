#lab2
bri <- read.csv("/Users/qinhao/Downloads/temp/Brisbane.csv")
str(bri)
head(bri)

#function load.file()，读入csv文件并将第一行时间转化为R的时间对象
load.file <- function(filename){
  location <- paste0("/Users/qinhao/Downloads/temp/",filename)
  x <- read.csv(location)
  x[,1] <- list(as.POSIXlt(x$time))
  x
}

#用lapply和load.file读取四个城市的数据
file_name <- list(f1 = "Brisbane.csv", f2 = "Cairns.csv", 
                  f3 = "Melbourne.csv", f4 = "Sydney.csv")
tem_dat <- lapply(file_name, load.file)

#求每个城市有多少行数据，通过nrow和mapply相结合
citydata_num <- mapply(nrow, tem_dat[1:4])
citydata_num

#城市记录的最高气温是多少
#先将每个城市的最高气温放在一个列表里，然后用sapply求出每个城市的最热的气温温
x <- list(tem_dat$f1$temp.max, tem_dat$f2$temp.max, tem_dat$f3$temp.max,tem_dat$f4$temp.max)
maxtemp <- sapply(x, max)
maxtemp

#估计每个城市的自相关函数，先对每个城市的temp求自相关函数，最高和最低气温类似
temp1 <- ts(tem_dat$f1$temp)
acf(temp1)

temp2 <- ts(tem_dat$f2$temp)
acf(temp2)

temp3 <- ts(tem_dat$f3$temp)
acf(temp3)

temp4 <- ts(tem_dat$f4$temp)
acf(temp4)







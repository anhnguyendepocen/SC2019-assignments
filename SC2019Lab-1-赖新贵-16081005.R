###lesson 1 R basics
  ##运用read.csv() 和 read.table()将数据以数据框的形式导入

    pools<-read.csv("E:/好好学习 天天向上/各科复习资料/统计计算/数据/swimming_pools.csv")
    pools<-read.table("E:/好好学习 天天向上/各科复习资料/统计计算/数据/swimming_pools.csv")

  ##导出pools数据并重新读取

    write.table(pools,"E:/好好学习 天天向上/各科复习资料/统计计算/table.pools.csv")
    table.pools<-read.table("E:/好好学习 天天向上/各科复习资料/统计计算/table.pools.csv")

    dput(pools,"E:/好好学习 天天向上/各科复习资料/统计计算/dput.pools.csv")
    dget.pools<-dget("E:/好好学习 天天向上/各科复习资料/统计计算/dput.pools.csv")

    save(pools,file="E:/好好学习 天天向上/各科复习资料/统计计算/save.pools.RData")
    #运用load函数获取表名
    load.pools<-load("E:/好好学习 天天向上/各科复习资料/统计计算/save.pools.RData")
    load.pools<-eval(parse(text = load.pools))#再转化为可执行对象

  ##数据框的子集操作

    pools[1,3]#第一行第三列对应元素
    pools[1,]#第一行所有元素
    pools$Longitude[1:3]#获取name这列元素的前三个元素
    subset(pools,Longitude<153,select = -Address)#选择维度小于153的，去除地址信息的子集

###lesson 2 Managing data frames with the dplyr package

  library(datasets)
  library(dplyr)

  ##选择Temp>80 & Month>5的观察
    filter(airquality,Temp>80 & Month>5)

  ##计算摄氏温度
    air<-mutate(airquality,celsius=(Temp-32)*5/9)

  ##计算每月平均温度
    group_by(air,Month) %>%
      summarise(meantemp=mean(Temp,na.rm = T))

  ##去除五月份的数据后按月份分组
    filter(airquality,Month!=5) %>%
      group_by(Month) %>%
      summarise(neabtemp=mean(Temp))

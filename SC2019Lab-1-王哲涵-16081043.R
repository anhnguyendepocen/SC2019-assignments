pools<-read.csv('C:/swimming_pools.csv') #使用read.csv()函数将swimming_pools.csv文件导入并命名为pools
pools
pools1<-read.table('C:/swimming_pools.csv') #使用read.table()函数将swimming_pools.csv文件导入并命名为pools
pools1
write.table(pools,"pools.csv") #使用write.table()函数将pools导出成csv文件格式
dput(pools,"pools1.csv") #使用dput()函数将pools导出成csv文件格式
save(pools,file="pools2.csv") #使用save()函数将pools导出成csv文件格式
pools2<-read.csv('C:/pools2.csv') #用read.csv读取保存的pools2.csv
pools$Name #提取pools name
pools$Address #提取pools address
pools[1,] #提取第一个pool的信息

airquality
library(dplyr) #加载dplyr包
filter(airquality, Temp > 80 & Month > 5) #用filter()找出5月之后华氏温度大于80的天数
airquality <- mutate(airquality, TempC = (Temp - 32)/1.8) #用mumate()函数添加新的列TempC将华氏温度转化为摄氏温度
head(airquality)
months <- group_by(airquality, Month) #按月进行分组
summarize(months, TempFmean = mean(Temp, na.rm = TRUE),
          TempCmean = mean(TempC, na.rm = TRUE) ) #计算每月华氏温度和摄氏温度的平均值
filter(airquality,Month > 5)%>% #删除5月的数据
  group_by(Month) %>% #按月进行分组
  summarize(Tempm = mean(Temp, na.rm = TRUE)) #计算每月温度平均值
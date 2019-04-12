x<-readLines('C:/BABAnews.txt',encoding="UTF-8") #读取文件 用encoding消除乱码
x #打印
str(x) #确定文章的段落数
library(glue)
trim(x)
nchar(x) #计算字符数
ee<-as.matrix(x) #将x转换为矩阵形式
y<-paste(ee,collapse = " ") #用paste函数将段落合并并且打印
regexpr("技术架构", x) #根据输出结果可知文章中含有技术架构
r <- regexpr("双11", x[1:5]) 
m <- regmatches(x[1:5], r)
d <- gsub("双11", "双十一", m)


x<-list(a="C:/temp/Bribane",b="C:/temp/Cairns",c="C:/temp/Melbourne",d="C:/temp/Syndey")
hottest<-function(name){ #创建新函数
  y<-read.csv("name") #读取文件
  max(temp.max) #计算最大值 
}
lapply(x,hottest)
debug(hottest)
#通过debug发现在 y<-read.csv("name")中多加了引号将name变成了字符串

x<-list(a="C:/temp/Bribane",b="C:/temp/Cairns",c="C:/temp/Melbourne",d="C:/temp/Syndey")
load.file<-function(name){ #创建新函数load.file
  a<-read.csv(name) #读取文件
  a <- mutate(a, TIME = as.POSIXlt(time)) #将时间格式转化
}

lapply(x,load.file)

numofrow<-function(name){ #创建新函数
  y<-read.csv(name) #读取文件
  nrow(y) #计算行数  
}
lapply(x,numofrow) 

hottest<-function(name){ #创建新函数
  y<-read.csv(name) #读取文件
  max(temp.max) #计算最大值 
}
lapply(x,hottest)



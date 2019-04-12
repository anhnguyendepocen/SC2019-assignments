#请使用函数load.file（）读取.csv文件，并使用as.posixlt将第一列（表示日期和时间的字符）转换为r time格式。 
load.file<-function(x,...) {
  A<-read.csv(x,...)
  A[,1]<-as.POSIXlt(A[,1])
  A
}


#然后使用lapply（）将load.file（）应用于每个文件名。
loc_file <- list(a="C:/temp/Brisbane.csv",b="C:/temp/Cairns.csv",c="C:/temp/Melbourne.csv",d="C:/temp/Sydney.csv")
lap.file<-lapply(loc_file,load.file)


#每个城市有多少行数据？
lapply(lap.file,nrow)


#城市记录的最热温度是多少？
lapply(lap.file,function(x) {
  max(x[,4])
})


#估计每个城市的自相关函数
lapply(lap.file,function(x) {
  for (i in 2:ncol(x))
  lm(x[,i] = x[,1], y ~ x, model = FALSE, 1:100)
})














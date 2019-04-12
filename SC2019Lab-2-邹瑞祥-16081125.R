load.file <- function(data) {
  pools<- read.csv(data)
  pools$time=as.POSIXlt(pools$time)
  pools
}
data<-lapply(c('Brisbane.csv','Cairns.csv','Melbourne.csv','Sydney.csv'),load.file)
max.file<-function(d)
{
  data.frame(d)
  max(d$temp.max)
}
acf.file<-function(d)
{
  data.frame(d)
  acflist<-lapply(d[,c(2:4)],acf)
  data.frame(sapply(acflist,function(acf){acf$acf}))
}
lapply(data,nrow)#rows
lapply(data, max.file)#hottest temp
lapply(data,acf.file)#autocorrelation function



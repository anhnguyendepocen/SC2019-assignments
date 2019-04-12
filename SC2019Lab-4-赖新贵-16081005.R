##练习调试工具

#构造函数data.interval：采用boostrap方法构造给定数据的置信区间

data.interval <- function(data,p,alpha,n,B)
{
   TBoot=NULL
   Sd.data=NULL
   for (i in 1:B)
   {
     data.sample<-sample(data,n,T)
     Tboot=quantile(data.sample,p)
     Tboot=c(TBoot,Tboot)
     Sd.data=c(Sd.data,sd(TBoot))
   }
   sd.quantile.data=sd(TBoot)
   Lcl <- quantile(data,p)+qnorm(alpha/2,0,1)*sd.quantile.data
   Ucl <- quantile(data,p)-qnorm(alpha/2,0,1)*sd.quantile.data
   return(c(Lcl,Ucl))
 } 
data.interval(rnorm(100),0.5,0.05,20,30)#错误信息显示存在没有使用的参数:Tboot

traceback()#显示函数出现data.interval错误但是无法明确具体哪步有问题

#接下来采用debug模式查看出错的具体位置
debug(data.interval)
data.interval(rnorm(100),0.5,0.05,20,30)
#按行运行程序时我们发现表达式：Tboot = c(TBoot, Tboot)存在问题，应该更正为TBoot = c(TBoot, Tboot)

#上一步发现了错误的根源，现在用trace函数将browser()插入到错误代码之前
trace("data.interval",browser,at=8)
untrace("data.interval")

#设置recover模式
options(error=recover) 
data.interval(rnorm(100),0.5,0.05,20,30)
#此处只有一个大函数，直接输入1会跳转到出错的地方

###################

#Profiling R

#计算调试后的函数各部分用时
data.interval <- function(data,p,alpha,n,B)
{
  TBoot=NULL
  Tboot=NULL
  Sd.data=NULL
  for (i in 1:B)
  {
  
    data.sample<-sample(data,n,T)
    Tboot<-quantile(data.sample,p)
    TBoot<-c(TBoot,Tboot)
    Sd.data=c(Sd.data,sd(TBoot))
  }
  sd.quantile.data=sd(TBoot)
  Lcl <- quantile(data,p)+qnorm(alpha/2,0,1)*sd.quantile.data
  Ucl <- quantile(data,p)-qnorm(alpha/2,0,1)*sd.quantile.data
  return(c(Lcl,Ucl))
} 
system.time(data.interval(rnorm(100),0.5,0.05,20,30))

#运用分析器
Rprof()
data.interval(rnorm(100),0.5,0.05,20,30)
Rprof(NULL)
summaryRprof("Rprof.out")
library(profr)
plot(parse_rprof("Rprof.out"))#绘制可视化图形观察哪部分用时长
#可以看到reduce，rs.getCompletionsSearchPath用时最长0.16s
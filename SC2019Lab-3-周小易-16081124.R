# Lab Session 3

# In this lab, you will play around text processing using R.
getwd()
setwd("C:/Users/Administrator/Desktop/Rdata/SC-L3")
# 1. Load text from the https://yanfei.site/docs/dpsa/BABAnews.txt and print it on
# screen. Text file contains some of the news of Alibaba.
babanews<-readLines("BABAnews.txt",encoding = "UTF-8")

# 2. How many paragraphs are there in the article?
babanews
#There are 5 paragraphs in the article

# 3. Trim leading whitespaces of each paragraph (try ??trim).
??trim
library(stringr)
babanews<-str_trim(babanews,"both")
babanews

# 4. How many characters are there in the article?
nchar(babanews)

# 5. Collapse paragraphs into one and display it on the screen (un-list it).
?unlist
?paste
?strwrap
babanews1<-paste0(babanews,collapse = "\n")
babanews1
str(babanews1)
?str_remove_all
babanews2<-str_remove_all(babanews1,pattern = "\n")
babanews2

# 6. Does the text contain word '技术架构'?
grepl("技术架构",babanews2)

# 7. Split the article into sentences (by periods).
?str_split
babanews3<-gsub(babanews2,pattern = "。")
babanews3

# 8. Replace '双11' with '双十一'.
babanews4<-gsub("双11|双\\s11\\s","双十一",babanews2)
babanews4


# In this lab, write your own code, enjoy the tools of debugging and profiling and
# write a short report of optimizing your code.

#
#使用Bootstrap方法估计置信区间
interval<- function(data,FUN,n,B,alpha=0.05)
{
  data.tl <- FUN(data)
  totaldata<-NULL
  for (b in 1:B) {
    Xsample <- sample(data,n,T)
    per.data <-FUN(Xsample)
    totaldata <- c(totaldata,per.data)
  }

  FinalSD <- sd(totaldata)
  
  #正态
  Lcl1 <- data.tl + qnorm(alpha/2,0,1)*FinalSD
  Ucl1 <- data.tl - qnorm(alpha/2,0,1)*FinalSD
  NORM.interval <- c(Lcl1,Ucl1)
  
  print('NORM.interval')
  print(NORM.interval)
  #枢轴量
  Lcl2 <- 2*data.tl - quantile(totaldata,1-alpha/2)
  Ucl2 <- 2*data.tl - quantile(totaldata,alpha/2)
  PIVOTAL.interval <- c(Lcl2,Ucl2)
  
  print('PIVOTAL.interval')
  print(PIVOTAL.interval)
  
  #分位数
  Lcl3 <- quantile(totaldata,alpha/2)
  Ucl3 <- quantile(totaldata,1-alpha/2) 
  Quantile.interval <- c(Lcl3,Ucl3)
  
  print('Quantile.interval')
  print(Quantile.interval)
}
library(TSA)
wd<-rnorm(10000,0,1)
wd<-exp(wd)
interval(wd,skewness,7000,7000)

debug(interval)
interval(wd,skewness,7000,7000)
undebug(interval)
system.time(interval(wd,skewness,7000,7000))

Rprof()
interval(wd,skewness,7000,7000)
Rprof(NULL)
summaryRprof()

#尝试修改循环代码
interval1<- function(data,FUN,n,B,alpha=0.05)
{
  data.tl <- FUN(data)
  totaldata<-NULL
   xlist<-NULL
   for (b in 1:B) {
     Xsample <- sample(data,n,T)
     xlist[[b]]<-Xsample
   }
   totaldata <- sapply(xlist,FUN)
  FinalSD <- sd(totaldata)
  #正态
  Lcl1 <- data.tl + qnorm(alpha/2,0,1)*FinalSD
  Ucl1 <- data.tl - qnorm(alpha/2,0,1)*FinalSD
  NORM.interval <- c(Lcl1,Ucl1)
  
  print('NORM.interval')
  print(NORM.interval)
  #枢轴量
  Lcl2 <- 2*data.tl - quantile(totaldata,1-alpha/2)
  Ucl2 <- 2*data.tl - quantile(totaldata,alpha/2)
  PIVOTAL.interval <- c(Lcl2,Ucl2)
  print('PIVOTAL.interval')
  print(PIVOTAL.interval)
  #分位数
  Lcl3 <- quantile(totaldata,alpha/2)
  Ucl3 <- quantile(totaldata,1-alpha/2) 
  Quantile.interval <- c(Lcl3,Ucl3)
  
  print('Quantile.interval')
  print(Quantile.interval)
}

wd<-rnorm(10000,0,1)
wd<-exp(wd)
interval1(wd,skewness,7000,7000)

system.time(interval1(wd,skewness,7000,7000))

Rprof()
interval1(wd,skewness,7000,7000)
Rprof(NULL)
summaryRprof()

Rprof()
interval(wd,skewness,7000,7000)
Rprof(NULL)
summaryRprof()

library(profvis)
profvis(interval(wd,skewness,7000,7000))

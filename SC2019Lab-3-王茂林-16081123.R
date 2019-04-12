setwd("C:\\Users\\hp\\Desktop\\大三下\\统计计算\\SC2019Lab-3-王茂林-16081123")
library(dplyr)
###############
#Lab Session 3#
###############
news <- readLines("BABAnews.txt",encoding = "UTF-8")#read txt encoded with UTF-8
length(news)#number of paragraphs
?trimws
news <- trimws(news,which = c("left"))#remove leading whitespace 
news
numchar <- sapply(news,nchar)
sum(numchar)#compute the number of char
news <- paste(news,collapse = '')#collapse paragraphs into one
unlist(news)#unlist
grep('技术架构',news)#judge whether there is a '技术架构'
r <- gregexpr("(.*?)[。|？]",news)#match 。 or ？
#r <- gregexpr("(.*?)[。|）](?!”)",news)
regmatches(news,r)#But str in “” are considered as sentences
gsub("双11|双 11","双十一",news)#Replace

###############
#Lab Session 4#
###############
library(profvis)

kafang<-function()
{
  x<-rnorm(10000)
  y <- rep(0,10000)
  for (i in 1:10000)
  {
    y[i] <- x[i]^2 
  }
  sum(y)#mean(y)
}
var_kafang <- function()
{
  mean <- rep(0,10000)
  for (i in 1:10000)
  {
    mean[i]<-kafang()
  }
  var_kf<-var(mean)
  var_kf
}
var_kafang()
debug(var_kafang)#result is fault
undebug(var_kafang)
#opt
profvis({mean <- rep(0,10000)
for (i in 1:10000)
{
  mean[i]<-kafang()
}
var_kf<-var(mean)
var_kf})#find for circulation waste a lot of time
insertkafang <- function(mean)
{
  mean<-kafang()
}

profvis({mean <- rep(0,10000)
kmean <- sapply(mean,insertkafang)
var_kf<-var(kmean)
var_kf})
#No significant change
var_kf

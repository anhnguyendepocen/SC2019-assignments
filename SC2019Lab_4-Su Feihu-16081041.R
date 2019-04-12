###Lab Session 4
###name: Su Feihu   StudentID:16081041


##In this lab, write your own code, enjoy the tools of debugging and profiling and
##write a short report of optimizing your code.

setwd("D:/study/statistical computing")
##my own code
sd.median <- function(exam, n = 20, B = 1000)
{
  median.exam<- median(exam)
  for (i in 1:B)
  {
    examsample <- sample(exam, n, T)
    Tboot <- median(examsample)
    TBoot <- c(TBoot, Tboot)
  }
  SD.TBoot <- sd(TBoot)
  interval <- c(median.exam-1.96*SD.TBoot, median.exam+1.96*SD.TBoot)
  return(interval)
}

l <- list(a <- rnorm(10000), b <- runif(10000, 0, 10))
lapply(l, sd.median)

##
##Error提示TBoot没有定义
traceback()    ##问题出在median(TBoot, Tboot)  TBoot not found

debug(sd.median)
lapply(l, sd.median)
##debugging in: FUN(X[[i]], ...)
#debug at #1: {
#sd.median <- function(exam, n = 20, B = 1000)
#{
#  median.exam<- median(exam)
#  for (i in 1:B)
#  {
#    examsample <- sample(exam, n, T)
#    Tboot <- median(examsample)
#    TBoot <- c(TBoot, Tboot)
#  }
#  SD.TBoot <- sd(TBoot)
#  interval <- c(median.exam-1.96*SD.TBoot, median.exam+1.96*SD.TBoot)
#  return(interval)
#}
#Browse[2]> n
#debug at #3: median.exam <- median(exam)
#Browse[2]> n
#debug at #4: for (i in 1:B) {
#examsample <- sample(exam, n, T)
#Tboot <- median(examsample)
#TBoot <- c(TBoot, Tboot)
#}
#Browse[2]> n
#debug at #6: examsample <- sample(exam, n, T)
#Browse[2]> n
#debug at #7: Tboot <- median(examsample)
#Browse[2]> n
#debug at #8: TBoot <- c(TBoot, Tboot)
#Browse[2]> n
#Error in median(TBoot, Tboot) : object 'TBoot' not found
##同样可以一步一步发现问题所在

undebug(sd.median)
options(error = recover) 
lapply(l, sd.median)

##修正错误，在函数中定义TBoot为NULL
sd.median <- function(exam, n = 20, B = 1000)
{
  median.exam<- median(exam)
  TBoot <- NULL
  for (i in 1:B)
  {
    examsample <- sample(exam, n, T)
    Tboot <- median(examsample)
    TBoot <- c(TBoot, Tboot)
  }
  SD.TBoot <- sd(TBoot)
  interval <- c(median.exam-1.96*SD.TBoot, median.exam+1.96*SD.TBoot)
  return(interval)
}
l <- list(a <- rnorm(10000), b <- runif(10000, 0, 10))
lapply(l, sd.median)

###R Profiler
system.time(lapply(l, sd.median))

library(plyr)
library(doMC)
system.time(lapply(l, sd.median))

Rprof()
sd.median <- function(exam, n = 20, B = 1000)
{
  median.exam<- median(exam)
  TBoot <- NULL
  for (i in 1:B)
  {
    examsample <- sample(exam, n, T)
    Tboot <- median(examsample)
    TBoot <- c(TBoot, Tboot)
  }
  SD.TBoot <- sd(TBoot)
  interval <- c(median.exam-1.96*SD.TBoot, median.exam+1.96*SD.TBoot)
  return(interval)
}
l <- list(a <- rnorm(10000), b <- runif(10000, 0, 10))
lapply(l, sd.median)
Rprof(NULL)
summaryRprof()
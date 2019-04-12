#######
# L 4 #
#######

setwd("D:/记忆/New Life/任务/大三下/统计计算")

#1.
BABAnews <- readLines("BABAnews.txt",encoding = "UTF-8");

#2.
length(BABAnews)
#There are five paragraphs in this article.

#3.
trimws(BABAnews)

#4.
len <- nchar(BABAnews)
sum(len)
#There are 967 characters in thsi article.

#5.
BABANEWS <- paste(BABAnews[1],BABAnews[2],BABAnews[3],BABAnews[4],BABAnews[5]) 
BABANEWS
#6.
grep("技术架构",BABAnews)
#Yes.

#7.
strsplit(BABANEWS,"。")

#8.
gsub("双11","双十一",BABAnews)


#######
# L 5 #
#######

#1.
library(plyr)
library(doMC)
library(foreach)
library(iterators)
library(parallel)
install.packages("doParallel")
library(doParallel)

?parallel
library(help = "parallel")

detectCores(logical = F)

registerDoMC(cores = detectCores(logical = F))
system.time(aaply(1:10000, 1, function(x) rnorm(1, mean = x), 
                  .parallel = TRUE))
debug(system.time)
debug(registerDoMC)
debug(detectCores)
undebug(system.time)
undebug(registerDoMC)
proc.time()

#The problem is in system.time() line3-9,it can't use many cpus but I don't know how to fix it.By the way there are other
#error happened.When I try to close RStudio ,it happed Error: unable to quit when browser is active,and the way to fix it
#is to finish the debug progress.

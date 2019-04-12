setwd("D:/ProgramProject/R/Lab5")
#1.来自The Art of R Programming的一个练习，结果运行过程发现了错误
library(pixmap)
#拉什莫尔山 总统山
mtrush1 <- read.pnm("mtrush1.pgm")
mtrush1
plot(mtrush1)
#原书函数,给罗斯福打码
blurpart <- function(img, rows, cols, q){
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow = lrows,ncol = lcols, runif(lrows*lcols))
  newimg@grey <- (1-q)*img@grey + q*randomnoise
  return(newimg)
}
blurpart(mtrush1,84:163,135:177,0.65)
#Error in (1 - q) * img@grey + q * randomnoise : non-conformable arrays
blurpart2 <-function(img, rows, cols, q){
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  rmnoise <- matrix(nrow = lrows,ncol = lcols, runif(lrows*lcols))
  newimg@grey[rows, cols] <- q*rmnoise 
  return(newimg)
}
mtrush2 <- blurpart2(mtrush1,84:163,135:177,0.65)
plot(mtrush2)

#2. Lab3
setwd("D:/ProgramProject/R/Lab3")
#读取文件并将第一列转换为标准格式
load.file <- function(file="Brisbane.csv"){
  f <- read.csv(file)
  f[,1] <- as.POSIXlt(f[,1])
  f
}
load.file()
#Warning message:
#In `[<-.data.frame`(`*tmp*`, , 1, value = list(sec = c(0, 0, 0,  :
#provided 11 variables to replace 1 variables

load.file <- function(file="Brisbane.csv"){
  f <- read.csv(file)
  f[[1]] <- as.POSIXlt(f[[1]])
  #f$time <- as.POSIXlt(f$time)
  f
}
load.file()

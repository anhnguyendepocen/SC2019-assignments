# Statistical Computing Homework9
# author:Yuchao Tao   StudentID:16081045

rm(list = ls())
library(RSpectra)
library(jpeg)
library(animation)
#设置读取路径
setwd("/Users/tyc_219/Desktop/python")
#读取图片
wyz <- readJPEG("wuyanzu.jpg")
#可以看出图片结构为三维矩阵
str(wyz)
#设置压缩向量
k <- seq(10, 200, 10)
png("Compresspics%03d.png")

compress_jpg <- function(pic, k, plots = TRUE)
{
  #如果压缩向量k里面的最小值小于等于1，报错
  if(min(k) <= 1) stop("The minimum of the vector must larger than 1!")
  #svds()函数在给定的mxn矩阵A的前提下，可以找到其最大的k奇异值和相应的奇异向量
  svd_message <- function(jpg, i)
  {
    r <- svds(jpg[,,1], i)
    g <- svds(jpg[,,2], i)
    b <- svds(jpg[,,3], i)
    return(list(r = r, g = g, b = b))
  }
  
  pic <- svd_message(pic, 200)
  if(plots == TRUE){
    sigma <- pic$r$d
    plot(1:length(sigma), sigma, xlab="i-th sigma", ylab="sigma", main="Singular Values")
    plot(1:length(sigma), cumsum(sigma)/sum(sigma), main="Cumulative Percent of Total Sigmas")
  }
  #返回在不同取值下的压缩后图片，取值越小图片约模糊
  for(m in k)
  {
    r <- pic$r$u[, 1:m] %*% diag(pic$r$d[1:m]) %*% t(pic$r$v[,1:m])
    g <- pic$g$u[, 1:m] %*% diag(pic$g$d[1:m]) %*% t(pic$g$v[,1:m])
    b <- pic$b$u[, 1:m] %*% diag(pic$b$d[1:m]) %*% t(pic$b$v[,1:m])
    pics <- array(0, c(nrow(r), ncol(r), 3))
    pics[,,1] <- r
    pics[,,2] <- g
    pics[,,3] <- b
    writeJPEG(pics, sprintf("picture_svd_%03d.png", m))
  }
}

compress_jpg(wyz, k, plots = TRUE)
dev.off()
bm.files <- sprintf("picture_svd_%03d.png", k)
im.convert(files = bm.files, output = "Compresspics.gif")


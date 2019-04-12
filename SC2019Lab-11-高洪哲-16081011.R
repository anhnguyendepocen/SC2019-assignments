setwd("D:/ProgramProject/R/Lab11")
library(jpeg)
#设置工作目录，加载jpeg package
jpgSVD <- function(jpg,n){ 
  
  r <- jpg[,,1]
  g <- jpg[,,2]
  b <- jpg[,,3]
  m <- min(nrow(r),ncol(r))
  r <- svd(r)
  g <- svd(g)
  b <- svd(b)
  

  colorsvd <- function(color,n){
    u <- color$u[,1:n]
    d <- diag(color$d[1:n])
    v <- t(color$v[,1:n])
    color <- u %*% d %*% v
    color[color<0] = 0
    color[color>1] = 1
    return(color)
  }
  
  recoverjpg <- function(n){
    r <- colorsvd(r,n)
    g <- colorsvd(g,n)
    b <- colorsvd(b,n)
    newjpg = array(0,c(nrow(r),ncol(r),3))
    newjpg[,,1] <- r
    newjpg[,,2] <- g
    newjpg[,,3] <- b
    return(newjpg)
  }
  
  
  n <- round(seq.int(5,floor(m/3),length.out = n))
  #多次尝试发现，秩的1/3基本就能很好的还原图像
  for (i in n) {
    newjpg <- recoverjpg(i)
    writeJPEG(newjpg,paste("rank","_",i,".jpg"),0.95)
  }
}
luffy <- readJPEG("luffy.jpg")
jpgSVD(luffy,8)
selfimg <- readJPEG("selfimage.jpg")
jpgSVD(selfimg,8)
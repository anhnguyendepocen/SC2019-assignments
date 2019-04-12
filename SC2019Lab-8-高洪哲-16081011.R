setwd("D:/ProgramProject/R/Lab8")

f <- function(x1,x2){
  return(x1^2+x2^2)
}#初始函数
nelder.mead <- function(func,n=2,simplex=list(c(1,1),c(1,2),c(2,2)),ep=1e-5,maxcounts=500){
  fd <- function(array){
    return(do.call(f,as.list(array)))
  }#计算f(x1,x2)的值
  order_simplex <- function(simplex){
    order_s <- list()
    for(i in 1:(n+1)){
      order_s[i] <- simplex[order.fx[i]]
    }
    return(order_s)
  }#按照函数值大小对单纯形内的点进行排序
  unls <- function(x){
    return(unlist(simplex[x]))
  }#用到较多的unlist(simplex)
  ca.x0 <- function(simplex){
    x0 <- c(0,0)
    for(i in 1:n){
      x0 <- x0+unls(i)
    }
    return(x0/n)
  }#计算x0
  Xm <- function(simplex){
    Xm <- matrix(unls(1:n)-unls(n+1),ncol = 2,byrow = T)
    return(Xm)
  }#判断条件矩阵
  counts=0
  msimplex <- list()
  while(0.5*abs(det(Xm(simplex)))>ep&&counts<maxcounts){
    fx <- sapply(simplex,fd)
    order.fx <- order(fx)
    simplex <- order_simplex(simplex)
    xn1 <- unls(n+1)
    x0 <- ca.x0(simplex)
    x1 <- unls(1)
    xr <- x0 + (x0 - xn1)
    counts = counts+1
    msimplex[counts] <- list(matrix(unls(1:(n+1)),byrow = T,ncol = 2))
    if(fd(xr)>=min(fx)&&fd(xr)<max(fx[fx!=max(fx)])){
      simplex[n+1] <- list(xr)
      next
    }else if(fd(xr)<min(fx)){
      xe <- x0+2*(xr-x0)
      if(fd(xe)<fd(xr)){
        simplex[n+1] <- list(xe)
        next
      }else{
        simplex[n+1] <- list(xr)
        next
      }
    }else{
      xc <- x0+0.5*(xn1-x0)
      if(fd(xc)<max(fx)){
        simplex[n+1] <- list(xc)
        next
      }else{
        sim <- NULL
        for(i in 1:(n+1)){
          sim[i] <- list((x1 +0.5*(unls(i)-x1)))
        }
        simplex <- sim
        next
      }
    }
  }
  return(msimplex)
}

debug(nelder.mead)

gif<-nelder.mead(f)



library(animation)
saveGIF({
  for (i in 1:length(gif)) {
    plot(-1:2,-1:2,type='n',xlab="x1",ylab='x2',main=paste("Times:",i,sep = ""))
    polygon(matrix(unlist(gif[i]),ncol = 2))
  }
})#没用contour



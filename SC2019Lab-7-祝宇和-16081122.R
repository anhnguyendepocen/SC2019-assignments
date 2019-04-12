## Nelder Mead
library(reshape2)
library(ggplot2)
library(animation)
setwd("D://BUAA//大三下//统计计算 康雁飞//pics")

# 工具函数：expression求值
solv <- function(express, factors, values) {
  if (length(factors) != length(values)) {return(NULL)}
  for (i in 1:length(factors)) {
    assign(factors[i], values[i])
  }
  solution <- eval(express)
  return(solution)
}

# 工具函数：绘图函数
# 输入单纯型所对应的三个点的list，输出标记着这三个点的ggplot
simple.pic <- function(list0 = list0) {
  x<-seq(-4,4,by=.05)
  y<-seq(-4,4,by=.05)
  "%!%" <- function(x, y) {x^2 + y^2}
  z<-outer(x,y,"%!%")
  colnames(z)<-y
  rownames(z)<-x
  map<-melt(z)
  head(map)
  Contour<-ggplot(map,aes(x=Var1,y=Var2,z=value),xlim(-4,4),ylim(-4,4))
  Contour<-Contour+geom_tile(aes(fill=value))#根据高度填充
  Contour<-Contour+scale_fill_gradientn(colours=rainbow(15))
  Contour<-Contour+stat_contour(breaks=c(0,1,2,3,4,5,10,15,20,30,40,50,60,80,100))#加上等高线
  Contour<-Contour+labs(x="X",y="Y",fill="Hight",title="x^2 + y^2")#修改标签
  Contour2 <- Contour + geom_point(aes(x=list0[[1]][1],y=list0[[1]][2]),color='black',size=5) + geom_point(aes(x=list0[[2]][1],y=list0[[2]][2]),color='black',size=5)+ geom_point(aes(x=list0[[3]][1],y=list0[[3]][2]),color='black',size=5)
  return(Contour2)
}

# 工具函数：输入一个带有三个点的list，输出这三个点围成的三角形的面积
triangle.area <- function(list0 = list0) {
  triangle <- matrix(c(list0[[1]],list0[[2]],list0[[3]]), 3, 2, byrow = T)
  triangle <- cbind(triangle, c(1,1,1))
  area <- abs(0.5 * det(triangle))
  return(area)
}

# 主函数：用Nelder Mead方法求一个二元函数的极小值，输入该函数的表达式、
# 变量序列、初始值向量1、初始值向量2、初始值向量3
nelder.mead.duo <- function(express, factors, start1, start2, start3) {
  
  list0 <- NULL # 用来保存单次迭代单纯型的三个顶点向量的list
  list1 <- NULL # 用来保存所有迭代结果
  k <- 1
  max.vec <- vector() # 用来保存单纯型中值最大的向量
  min.vec <- vector() # 用来保存单纯型中值最小的向量
  x0 <- vector() # 用来保存中点
  xr <- vector() # 用来保存主步骤中的新点xr
  xc <- vector() # 用来保存contradiction point
  xe <- vector() # 用来保存第一种情况的新点xe
  
  list0[[1]] <- start1
  list0[[2]] <- start2
  list0[[3]] <- start3 # 三组初始值入列表
  
  # Nelder Mead 主循环
  while (triangle.area(list0) > 10^(-4)) {
    
    # list0中的三个向量即为三组values，选出最大最小函数值所对应的向量，
    # 并将最大函数值对应的向量从list中删除
    ys <- c(solv(express,factors,list0[[1]]),solv(express,factors,list0[[2]]),solv(express,factors,list0[[3]]))
    max.vec <- list0[[which.max(ys)]]
    min.vec <- list0[[which.min(ys)]]
    list0[[which.max(ys)]] <- NULL
    
    x0 <- (list0[[1]]+list0[[2]])/2 # 求得x0
    xr <- x0 + x0 - max.vec #求得主步骤的新点xr
    
    # 开始判断新点xr所对应的函数值在新单纯型中的大小排序位置
    if (solv(express,factors,xr) < solv(express,factors,list0[[1]]) & solv(express,factors,xr) < solv(express,factors,list0[[2]])) {
      #当xr所对应的函数值最小时
      xe <- x0 + 2*xr - 2*x0
      if (solv(express,factors,xe) < solv(express,factors,xr)) {
        list0[[3]] <- xe
        list1[[k]] <- list0
        k <- k+1
        ggsave(simple.pic(list0),filename = paste(k-1,".jpeg",sep = ""),width = 12,height = 11)
        next
      }
      else {
        list0[[3]] <- xr
        list1[[k]] <- list0
        k <- k+1
        ggsave(simple.pic(list0),filename = paste(k-1,".jpeg",sep = ""),width = 12,height = 11)
        next
      }
    }
    else if (solv(express,factors,xr) > solv(express,factors,list0[[1]]) & solv(express,factors,xr) > solv(express,factors,list0[[2]])) {
      #当xr所对应的函数值最大时
      xc <- x0 + 0.5*max.vec - 0.5*x0 # Contradiction Point
      if (solv(express,factors,xc) < solv(express,factors,max.vec)) {
        list0[[3]] <- xc
        list1[[k]] <- list0
        k <- k+1
        ggsave(simple.pic(list0),filename = paste(k-1,".jpeg",sep = ""),width = 12,height = 11)
        next
      }
      else {
        mid1 <- (list0[[1]] + list0[[2]]) / 2
        mid2 <- (max.vec + min.vec) / 2
        list0 <- NULL
        list0[[1]] <- mid1
        list0[[2]] <- mid2
        list0[[3]] <- min.vec
        list1[[k]] <- list0
        k <- k+1
        ggsave(simple.pic(list0),filename = paste(k-1,".jpeg",sep = ""),width = 12,height = 11)
        next
      }
    }
    else {
      #当xr的函数值大小排名第二时
      list0[[3]] <- xr
      list1[[k]] <- list0
      k <- k+1
      ggsave(simple.pic(list0),filename = paste(k-1,".jpeg",sep = ""),width = 12,height = 11)
      next
    }
  }
  return(list1)
}

# 测试函数
nelder.mead.duo(expression(x1^2 + x2^2), c("x1","x2"), c(1,1), c(1,2), c(2,2))
# Results
#[[21]]
#[[21]][[1]]
#[1] 0 0
#
#[[21]][[2]]
#[1] -0.001381338  0.015684009
#
#[[21]][[3]]
#[1] 0.006807342 0.011079043

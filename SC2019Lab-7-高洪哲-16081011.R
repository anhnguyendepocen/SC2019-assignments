setwd("D:/ProgramProject/R/Lab7")
#Lab6 
newton.raphson <- function(x=c(5,6),f=expression(x1^2-x1*x2+x2^2+exp(x2)),para=c("x1","x2"),ep=1e-10,countsmax=10000){
  dx <- function(expr){
    d <- deriv(expr,para,hessian = T,function.arg = T)
    m <- do.call(d,as.list(x))
    dx1 <- attr(m,"gradient")
    dx2 <- attr(m,"hessian")
    dx1 <- matrix(dx1,nrow = 2)
    dx2 <- matrix(dx2,nrow = 2,byrow = T)
    return(solve(dx2,dx1))
  }
  result <- function(x1,x2){
    return(eval(f))
  }
  counts = 0
  repeat{
    x <- x-dx(f)
    counts = counts +1 
    re <- do.call(result,as.list(x))
    if(abs(re)<ep||counts>=10000){
      return(list(counts=counts,root=x))
      break
    }
  }
}
newton.raphson()

#Lab7
#logistic regression 
logisticR <- function(beta,x,y){
  x <- cbind(1,x)
  beta <- matrix((rep(beta,nrow(x))),ncol = length(beta),byrow = T)
  para <- rowSums(-beta*x)
  p <- 1/(1+exp(para))
  logL = sum(y*log(p)+(1-y)*log(1-p))
  return(-logL)
}
library(faraway)
wcgs$y <- ifelse(wcgs$chd=="no",0,1)
y <- wcgs$y
x <- matrix(c(wcgs$height,wcgs$cigs),ncol = 2)
lmod <- glm(y~height+cigs,data = wcgs,family = binomial)
lmod$coefficients
o1 <- optim(c(-4,0.05,0.04),x=x,y=y,logisticR,method = 'Nelder-Mead')#默认方法
#通过单纯型的方式不断替换函数的最差的顶点从而得到最优值。
#因为没有用的梯度故不是非常有效，但方法十分稳健，效率也不低，因此被作为默认算法。
o2 <- optim(c(-4,0.05,0.04),x=x,y=y,logisticR,method = "BFGS")
#BFGS:是一种拟牛顿法，也称变尺度法。
#该算法改进了牛顿法中容易受初值的影响的弱点，
#但是又不需要在每一步优化的过程中计算精确的hessian矩阵及其逆矩阵，
#在具备牛顿法搜索快的特性的基础上又能有效的搜索全局最优解，
#一次使用十分广泛，是optim函数中应用最广的算法。
o3 <- optim(c(-4,0.05,0.04),x=x,y=y,logisticR,method = "L-BFGS-B")
#是对BFGS算法的一个优化，能够在优化的同时增加箱型约束条件，
#一定程度上增强了这些无约束的非线性规划方法的功能。
o1$par
o2$par
o3$par

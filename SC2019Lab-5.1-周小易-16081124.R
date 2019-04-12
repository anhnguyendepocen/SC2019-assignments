#lab 5.1
#Multivariate Newton-Raphson Method
#FUN: Function for evaluating expressions
#x0 : initial value,a vector or not
#tol: the tolarance of error
#n  : The maximum number of iterations
newton.raphson<-function(FUN,x0,tol = 1*10^(-5),n = 1000){
  library(numDeriv)
  x<-x0
  k<-0
  
  while ( k<n ) {
    hm<-hessian(func = FUN,x)  #hessian矩阵
    df<-grad(FUN,x)  #一阶导数
    x1<-x
    x<-x-solve(hm) %*% df  #迭代求解
    
    k<-k+1
    if(abs(FUN(x1)-FUN(x))<tol) break

  }
  if(abs(FUN(x1)-FUN(x))<tol) cat("函数值变化已小于",tol,sep="")
  if(k==n) cat("已经经过", n ,"次迭代,未能收敛",sep="")
  cat("\n")
  return(list(x=x,k=k,y=FUN(x)))
}


FUN <- function(x){
  x[1]^2-x[1]*x[2]+x[2]^2+exp(x[2])  #输入多元函数表达式
}

newton.raphson(FUN=FUN,x0=c(1,2))

FUN <- function(x){
  x[1]^2-5
}

newton.raphson(FUN=FUN,x0=2)



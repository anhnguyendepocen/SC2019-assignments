setwd("D:/ProgramProject/R/Lab6")
newton.raphson <- function(x=1,f=expression(x^2-5),ep=1e-8,countsmax=1000){
  d <- D(f,"x")
  counts = 0
  repeat{
    x <- x-eval(f)/eval(d)
    counts = counts +1
    if(abs(eval(f))<ep||counts>=countsmax){
      return(list(counts=counts,root=x))
      break
    }
  }
}

#1 设置的默认值就是这个函数 Root=2.236068
newton.raphson()
#2 初始值就是函数的根   Root=0.25
newton.raphson(x=0.25,f=expression(sqrt(sqrt(x^2))))
#3 Root = 0.4303877 
newton.raphson(x=0.5,f=expression(x*exp(-x^2)-0.4/(1+exp(x))-0.2))#四次
newton.raphson(x=0.6,f=expression(x*exp(-x^2)-0.4/(1+exp(x))-0.2))#五次



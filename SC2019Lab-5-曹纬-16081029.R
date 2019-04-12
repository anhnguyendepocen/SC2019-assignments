#1Write R code to find the root of x^2=5

##method1
D(expression(x^2),"x") ##对X^2求导
newtonraphson <- function(x,e){##输入初始值和误差两个参数
  repeat{
    f <- x^2-5
    d <- 2*x
    x <- x - f/d
    if(abs(f)<e)##当误差小于e时，跳出循环
      break
  }
  return(x)
}
newtonraphson(2,10^-10)
##method2 更为通用的函数
Newtonraphson <- function(x,fun,e){
  repeat{
    f <- eval(fun)
    d <- eval(D(fun,"x"))
    x <- x - f/d
    if(abs(f)<e)
      break
  }
  return(x)
}
fun <- expression(x^2-5)
Newtonraphson(2,fun,10^-10)

#2 to find the root of sqrt(abs(x))=0

f <- function(x)(sqrt(abs(x)))
uniroot(f,c(0,1))

Newtonraphson <- function(x,fun,e){
  repeat{
    f <- eval(fun)
    d <- 0.5*abs(x)^(-0.5)
    x <- abs(x - f/d)
    if(abs(f)<e)
      break
  }
  return(x)
}

#3  find the root of x*exp(-x^2)=0.4*(exp(x)+1)^-1+0.20

##method1
D(expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1-0.20),"x")
newtonraphson <- function(x,e){
  repeat{
    f <- x*exp(-x^2)-0.4*(exp(x)+1)^-1-0.20
    d <- exp(-x^2) - x * (exp(-x^2) * (2 * x)) + 0.4 * ((exp(x) + 1)^-(2) * exp(x))
    x <- x - f/d
    if(abs(f)<e)
      break
  }
  return(x)
}
newtonraphson(0.5,10^-10)
newtonraphson(0.6,10^-10)
##method2
Newtonraphson <- function(x,fun,e){
  repeat{
    f <- eval(fun)
    d <- eval(D(fun,"x"))
    x <- x - f/d
    if(abs(f)<e)
      break
  }
  return(x)
}

fun <- expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1-0.20)
Newtonraphson(0.5,fun,10^-10)
Newtonraphson(0.6,fun,10^-10)






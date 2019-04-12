
NR <- function(x,f,Epsilon = 10^-10)#递归法
{
  if (abs(eval(f)) < Epsilon )
    x
  else{  
    x <- x- eval(f)/eval( D(f, "x"))
    NR(x,f)  
  }
}
NR2 <- function(x,f,Epsilon = 10^-10)#循环法
{
  while (abs(eval(f)) > Epsilon ){
    x <- x- eval(f)/eval( D(f, "x"))

  }
  x
}
f <-expression(x^2-5)
NR2(-5,f)#只能得到单根(-5,-2.24)
g <- expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1-0.2)
NR(2,g)#(0.5,0.4303)(0.6,0.4303)(2,1.1778)
h <-expression(sqrt(sign(x)))
NR(0.25,h)#微分表里没有这个函数'sign'

NR.abs <- function(x,Epsilon = 10^-10)
{
  if(x>0){
    f <-expression(sqrt(x))
  }
  else{  
    f <-expression(sqrt(-1*x))
  }
  if (abs(eval(f)) < Epsilon )
    x
  else{  
    x <- x- eval(f)/eval( D(f, "x"))
    NR.abs(x)  
  }
}
NR.abs(0.3)#函数不适合牛顿法
debug(NR.abs)
NR.new <- function(f,a,b,Epsilon = 10^-6){#得到所有根
  result <-c()
  for(num in 1:1000)
    {x <-runif(1,a,b)#随机生成初始值
      while (abs(eval(f)) > Epsilon )
        {
        x <- x- eval(f)/eval( D(f, "x"))
        if (x<a|x>b)
          {
          x<-NULL
          break
          }
        }
    if (is.null(x)==FALSE)
        {result <-c(result,round(x,5))}
   }
  index<-duplicated(result)
  result[!index]#取消重复值
}
NR.new(g,0,2)
undebug(NR.new)

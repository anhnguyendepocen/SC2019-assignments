###SC2019Lab-5-Shuaidong Zhu-16081133

##Construct a function that returns the value of the expression (given x)
fval <- function(FUN,x){
  return(eval(FUN))
}
##Newton-Raphson Method
newton.raphson <- function(x,FUN,miu = 0,N = 10000){
  dx <- D(FUN,"x")#Derivative
  i = 0
  value = fval(FUN,x)
  valuedx = fval(dx,x)
  while (i <= N & abs(value) > miu )
  {   
      x = x - value/valuedx #Iteration
      i = i +1
      value = fval(FUN,x)
      valuedx = fval(dx,x)
  }     
  return(x)
}

#First function
f1 <-  expression(x^2-5)
a1 <- newton.raphson(2,f1)
a1

#Second function
f2 <- expression(sqrt(sqrt(x^2)))
a2 <- newton.raphson(0.25,f2,0,10000)
a2
a3 <- newton.raphson(-0.25,f2)
a3

#Third function
f3 <- expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2)
a4 <- newton.raphson(0.5,f3,0,10000)
a4
a5 <- newton.raphson(0.6,f3,0,10000)
a5
a6 <- newton.raphson(2,f3,0,10000)
a6




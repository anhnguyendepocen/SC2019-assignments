#Statistical Computing Homework4
#author:Yuchao Tao  StudentID:16081045

#Write the function newton.rephson() , x0 is the initial x value, 
#fun is the f(x), precision is the estimated accuracy
newton.rephson <- function(x0, fun, precision)
{
  #Compute derivatives of simple expressions, symbolically and algorithmically
  dx <- D(fun, "x")
  #Build a vector to storage each iteration x value
  estimate_x <- vector(mode = "numeric", length = 0)
  estimate_x[1] <- x0
  x <- x0
  #Evaluate an R expression in a specified environment
  y <- eval(fun)
  i <- 1
  #Establish a loop until the estimated accuracy reaches the specified level
  while(abs(y) > precision)
  {
    i <- i+1
    dy <- eval(dx)
    x <- x - y/dy
    y <- eval(fun)
    estimate_x[i] <- x - y/dy
  }
  #Return each iteration x value, the last one is the number closest to the zero point of the f(x)
  return(estimate_x) 
}

newton.rephson(1.5, expression(x^2 - 4), 10^(-8))

#This absolute value function cannot be derived at zero point, it cannot be found using a function.
newton.rephson(0.25, expression(sqrt(abs(x))), 10^(-8))
newton.rephson(0.25, expression(sqrt(sqrt(x^2))), 10^(-8))

newton.rephson(0.5, expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1 - 0.2), 10^(-8))
newton.rephson(0.6, expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1 - 0.2), 10^(-8))





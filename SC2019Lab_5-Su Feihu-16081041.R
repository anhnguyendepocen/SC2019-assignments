###SC2019-Lab Session 5
###name: Su Feihu  StudentID: 16081041


##function to find the root by Newton method
Newton.raphson <- function(fun, x0, tolerancelevel = 10 ^ (-10))
{
  dx <- D(fun, "x")
  x <- x0
  y <- eval(fun)
  while (abs(y) > tolerancelevel)
  {
    yy <- eval(dx)
    x <- x - y / yy
    y <- eval(fun)
  }
  return(x)
}

#find the root of x^2=5
Newton.raphson(expression(x ^2 - 5), 2)

#find the root of x*exp(-x^2)=0.4*(exp(x)+1)^-1 + 0.2
#Try initial values 0.5 and 0.6
Newton.raphson(expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1 - 0.2), 0.5)
Newton.raphson(expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1 - 0.2), 0.6)

#find the root of f(x)= √|x|,initial value 0.25
Newton.raphson(expression(sqrt(abs(x))), 0.25)
##Error in D(fun, "x") : 微分表里没有这个函数'abs';
#there is no Differential function for absolute value function 
#try to give the Differential function to function
Newton.raphson2 <- function(fun, x0, tolerancelevel = 10 ^ (-10), dx = NULL)
{
  if (is.null(dx))
  {
    dx <- D(fun, "x")
  }
  x <- x0
  y <- eval(fun)
  while (abs(y) > tolerancelevel)
  {
    yy <- eval(dx)
    x <- x - y / yy
    y <- eval(fun)
  }
  return(x)
}
Newton.raphson2(expression(sqrt(abs(x))), 0.25, 10 ^ (-10),
               expression(sign(x)*(0.5 * abs(x)^-0.5)))
#no result
#special case, in each step, you just get the opposite of last x.

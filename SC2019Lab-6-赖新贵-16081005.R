
#search for the minimum of function y=x^2-x*y+y^2+e^{y}

Multinewtonrephson <- function(f,x,y,e){
  #Newton¡¯s algorithm for multidimensional optimization
  #"f" is the expression of function
  #"x" and "y" are the value of variables
  #"e" is the accuracy
  n = 1
  X <- matrix(c(x,y),2)
  while (n>0) {
    first <- DF(f,x,y)$first
    second <- DF(f,x,y)$second
    X <- X-solve(second,first)
    x <- X[1]
    y <- X[2]
    if (all(abs(first) < e))
      break
    else 
      n <- n+1
  }
  return(list(variable = c(x,y),
              first.derivative = first,
              second.derivative = second,
              n = n))
}

DF <- function(f,x,y){
  #compute the gradient vector and hessian matrix
  #"f" is the expression of function
  #"x" and "y" are the value of variables 
  df <- deriv(f,c("x","y"),hessian = T,func = T)
  dfx <- df(x,y)
  a = matrix(as.vector(attr(dfx,"hessian")),nrow = 2)
  b = matrix(attr(dfx,"gradient"),nrow = 2)
  return(list(first = b,second = a))
}

f <- expression(x^2-x*y+y^2+exp(y))
x=-10:10
y=-10:10
eval(f)
Multinewtonrephson(f,1,1,1e-10)

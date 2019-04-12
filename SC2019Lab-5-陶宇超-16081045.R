#Statistical Computing Homework5
#author:Yuchao Tao  StudentID:16081045

#Write the function newton.rephson() , x0 is the initial x value, 
#fun is the f(x), precision is the estimated accuracy
newton.raphson <- function(x1, x2, fun, precision, max.loop)
{
  #Compute derivatives of simple expressions, symbolically and algorithmically
  dy <- deriv(fun, c("x1", "x2"), hessian = TRUE)
  x1 <- x1
  x2 <- x2
  
  #Build a vector to storage each iteration x value
  estimate_x1 <- vector(mode = "numeric", length = 0)
  estimate_x1[1] <- x1
  estimate_x2 <- vector(mode = "numeric", length = 0)
  estimate_x2[1] <- x2
  
  #Evaluate an R expression in a specified environment
  d <- eval(dy)
  i <- 1
  #Establish a loop until the estimated accuracy reaches the specified level
  while((abs(d) > precision) && (i < max.loop))
  {
    i <- i+1
    d <- eval(dy)
    hessian <- matrix(attributes(d)$hessian, byrow = F, 2, 2)
    x1 <- x1 - solve(hessian, t(attributes(d)$gradient))[1,]
    x2 <- x2 - solve(hessian, t(attributes(d)$gradient))[2,]
    estimate_x1[i] <- x1
    estimate_x2[i] <- x2
  }
  #Return each iteration x value, the last one is the number closest to the zero point of the f(x)
  return(list(Iteration_x1 = estimate_x1, Iteration_x2 = estimate_x2))
}

newton.raphson(1, 2, expression(x1^2 - x1*x2 + x2^2 + exp(x2)), 10^(-6), 100)


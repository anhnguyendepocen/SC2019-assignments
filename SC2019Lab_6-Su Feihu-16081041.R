###Lab Session 6
###NAME: Su Feihu  StudentID: 16081041

##function of newton method for multidimensional optimization
Newton.mult.raphson <- function(fun, x10, x20 ,df = NULL, hessian = NULL, tol = 0.000001, maxiter = 10000)
{
  niter <- 0   #number of loops
  x1 <- x10
  x2 <- x20   # initial value
  if (is.null(df) & is.null(hessian))    ##there is no input of df and hessian
  {
    d <- deriv(fun, c("x1", "x2"), hessian = TRUE)
    df <- expression(attributes(eval(d))$gradient)  #the function of df
    hessian <- expression(matrix(attributes(eval(d))$hessian, byrow = F, 2,2)) #the function of hessian
  }
  if (is.null(hessian) & !is.null(df))  ##there is no input of hessian
  {
    d <- deriv(fun, c("x1", "x2"), hessian = TRUE) 
    hessian <- expression(matrix(attributes(eval(d))$hessian, byrow = F, 2,2))#the function of hessian
  }
  if (is.null(df) & !is.null(hessian))  ##there is no input of df 
  {
    d <- deriv(fun, c("x1", "x2"), hessian = TRUE)
    df <- expression(attributes(eval(d))$gradient)   #the function of df
  }
  vector_x <- matrix(c(x1, x2))  #create a vector to save initial value and solution
  k <- c(0, eval(fun))  #create a 2*1 matrix to make stopping rules(f(n+1)-f(n))
  while (abs(k[2] - k[1]) > tol & niter <= maxiter)  #stopping rules
  {
    niter = niter + 1   # add number of loop
    vector_x <- vector_x - solve(eval(hessian), t(eval(df))) #finding next x
    x1 <- vector_x[1,1]
    x2 <- vector_x[2,1]
    k[1] <- k[2]
    k[2] <- eval(fun)
  }
  if (niter > maxiter){
    warning("Maximum number of iterations 'maxiter' was reached.")
  }
  return(vector_x)
}
  
fun = expression(x1^2 -x1*x2 + x2^2 + exp(x2))
Newton.mult.raphson(fun, 1,2)  
  

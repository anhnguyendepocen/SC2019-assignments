#minimize the y = x1^2-x1*x2+x2^2+exp(x2)
#fexp - function expression, xvec - the variables vector for the function expression
#x0 - the initial value for the newton method, tlevel - tolerance level
mul_nrfun <- function(fexp,xvec,x0,tlevel = 1e-10){
  x <- NULL
  xn <- NULL
  n <- 0
  
  #function to get the value from the function expression
  f <- function(x){
    x1 = x[1]
    x2 = x[2]
    eval(fexp)
  }
  
  #function to calculate the sqrt for sum of squares
  sum_square <- function(x){
    sqrt(sum(x))
  }
  
  if(is.null(fexp)) print("the expression of function couldn't be NULL")
  else{
    dfx <- deriv(fexp, namevec = xvec, func = T, hessian = T)
    x = x0
    grad = 1
    
    while(abs(grad) >= tlevel | n < 10000){
      df <- dfx(x[1],x[2])
      #the gradient of fx
      g <- attributes(df)$gradient
      grad = sum_square(g)
      
      h <- attributes(df)$hessian
      hmatrix <- matrix(h, length(xvec),length(xvec))#reshape the matrix
      #hmatrix_1 is the inv of the hessian matrix
      hmatrix_1 <- solve(hmatrix)
      
      xn <- x - hmatrix_1%*%t(g)
      x <- xn
      n = n+1
    }
    print(c(" when x = ",round(x, digits = 7)))
    #according to the f(xn) to judge the maximum or minimum occasion
    if(f(x) >= f(x0)){
      print(paste("the function get the maximum",round(f(x), digits = 7))) 
    }else
      print(paste("the function get the minimum",round(f(x), digits = 7)))
  }
}
debug(mul_nrfun)
undebug(mul_nrfun)

y = expression(x1^2-x1*x2+x2^2+exp(x2))
xvec = c("x1","x2")
x0 <- c(1,1)

mul_nrfun(y, xvec, x0)#optimize the y at initial value x0

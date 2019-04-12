###SC2019Lab Session 7. 8 
###Name:Su Feihu   StudentID:16081041


setwd("D:/study/statistical computing")
##newton method to find the Maximum or Minimum of the function for Multidimensional Optimization
newton <- function(init.value, namevec, fun, gradient = NULL, hessian = NULL, tol = 1e-5, maxiter = 1000)
{
  #fun is the function to  Optimization
  #init.value is the initial value of variables
  #namevec is the vector ofnames of variables
  #gradient hessian is the gradient vector and Hessian matrix
  #tol is the tolerance level of stopping rule
  #maxiter is the max of iteration
  if(length(namevec) != length(init.value)){    ##initial value must same with names vector of varibles
    stop("namevec must have the same length with initial value")
  }
  niter <- 0   #number of loops
  vector_x <- init.value
  if (is.null(gradient) & is.null(hessian))    ##there is no input of df and hessian
  {
    df <- deriv(fun, namevec, hessian = TRUE, function.arg = TRUE)
    vector_x <- matrix(vector_x)  
    k <- c(0, 10)  #create a 2*1 matrix to make stopping rules(f(n+1)-f(n))
    while (abs(k[2] - k[1]) > tol & niter <= maxiter)  #stopping rules
    {
      niter = niter + 1   # add number of loop
      value <- do.call(df, as.list(vector_x))
      hessian <- matrix(attributes(value)$hessian, byrow = F, sqrt(length(attributes(value)$hessian)))
      gradient <- t(attributes(value)$gradient)
      vector_x <- vector_x - solve(eval(hessian), eval(gradient)) #finding next x
      value <- do.call(df, as.list(vector_x))
      k[1] <- k[2]
      k[2] <- value[1]
    }
    if (niter > maxiter){
      warning("Maximum number of iterations 'maxiter' was reached.")
    }
    return(vector_x)
  }
  else if (!is.null(hessian) & !is.null(gradient))
  {
    k <- c(0, 10)
    vector_x <- matrix(vector_x)
    while(abs(k[2] - k[1]) > tol & niter <= maxiter)
    { 
      niter <- niter + 1
      gradient <- grad(as.vector(vector_x))# get the gradient matrix 
      hessian <- hess(as.vector(vector_x))# get the hessian matrix
      vector_x = vector_x - solve(hessian, gradient)# calculate the step 
      k[1] <- k[2]
      k[2] <- fun(as.vector(vector_x))
    } 
    if (niter > maxiter){
      warning("Maximum number of iterations 'maxiter' was reached.")
    }
    return(vector_x)  #return the result
  } ## hessian and gradient 
  else{return("Input the complete derivative expression or none")}
}


fun = expression(x1^2 -x1*x2 + x2^2 + exp(x2))
newton(c(1, 2), c("x1", "x2"), fun)  
fun <- function(Beta){
  x1 <- beta[1]
  x2 <- beta[2]
  x1^2 -x1*x2 + x2^2 + exp(x2)
}

grad <- function(beta){
  x1 <- beta[1]
  x2 <- beta[2]
  matrix(c(2*x1-x2, -x1+2*x2+exp(x2)),2,1)
}

hess <- function(beta){
  x1 <- beta[1]
  x2 <- beta[2]
  matrix(c(2, -1, -1, 2+exp(x2)),2,2)
}
beta <- c(10,10)
newton(c(1, 2), c("x1", "x2"), fun(beta), grad(beta), hess(beta))


###data
library(faraway)
data(wcgs, package = "faraway")
plot(height ~ chd, wcgs)
wcgs$y <- ifelse(wcgs$chd == "no", 0, 1)


###use the function to solve
x = wcgs$cigs
y = wcgs$y
fun <- function(Beta){
  Beta0 <- beta[1]
  Beta1 <- beta[2]
  sum(y*(Beta0 + Beta1*x) - log(1+exp(Beta0 + Beta1*x)))
}

grad <- function(beta){
  Beta0 <- beta[1]
  Beta1 <- beta[2]
  matrix(c(sum(y-exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x))),
           sum(x(y-exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x))))),2,1)
}

hess <- function(beta){
  Beta0 <- beta[1]
  Beta1 <- beta[2]
  matrix(c(-sum(exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x)^2)),
           -sum(x*exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x)^2)),
           -sum(x*exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x)^2)),
           -sum(x^2*exp(Beta0 + Beta1*x)/(1+exp(Beta0 + Beta1*x)^2))),2,2)
}

beta <- c(1,1)
newton(c(1,1), c("beta1", "beta2"), fun(beta), grad(beta), hess(beta))



######Use optim() to carry out maximum likelihood for the Logistic regression model.
x = wcgs$cigs
y = wcgs$y
fun <- function(Beta){
  sum(y*(Beta[0] + Beta[1]*x) - log(1+exp(Beta[0] + Beta[1]*x)))
}
re<-optim(c(1,1), fun, control = list(fnscale = -1))
re$par
fit <- glm(y ~ cigs, family=binomial, data = wcgs)
summary(fit)
fit$coefficients

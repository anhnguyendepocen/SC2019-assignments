###SC2019Lab-6-Shuaidong Zhu-16081133

##Load Package numDeriv
library(numDeriv)

#Newton’s algorithm for multidimensional optimization 
newton_multoptim <- function(fun,x,tol = 1e-5, maxiter = 1000){
  niter <- 0
  hm <- hessian(fun,x)#Hessian matrix
  df <- grad(fun,x)  #first derivative    
  while(niter <= maxiter)
  {
    f.value1 <- fun(x)
    niter <- niter + 1
    x <- x - solve(hm,df)
    hm <- hessian(fun,x)
    df <- grad(fun,x)
    f.value2 <- fun(x)
    if(abs(f.value2-f.value1) < tol) #|f(xn)-f(xn-1)|< e
      break
  }
  if (niter > maxiter) {
    warning("Maximum number of iterations 'maxiter' was reached.")
  }
  extrm.fx <- fun(x)
  return(list(extrm.root = x, extrm.fx = extrm.fx, niter = niter, estim.prec = df))
}


#Apply (minimise  y <- x[1]^2-x[1]*x[2] + x[2]^2 + exp(x[2]))
f1 <- function(x){
  y <- x[1]^2-x[1]*x[2] + x[2]^2 + exp(x[2])
  return(y)
}

newton_multoptim(f1,c(1,1))
# $extrm.root
# [1] -0.2162814 -0.4325628
# 
# $extrm.fx
# [1] 0.789177
# 
# $niter
# [1] 4
# 
# $estim.prec
# [1] 0.000000e+00 8.507409e-09


      
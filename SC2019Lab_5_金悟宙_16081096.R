n=0
x1=3
x2=3
f <- expression(x1^2-5)
repeat{
  x2 <- x1-(x1^2-5)/eval(D(f,"x1"))
  x1 <- x2
  d <- 5-x2^2
  n <- n+1
  if(d<1e-10){
    return(x2)
    break
  }
}
print(x2)



#显然，x=0，而且在x=0处不可导#



f <- expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2)
newton.raphson <- function(x,f,eps=1e-10){
  repeat{
  d <- eval(f)
  df <- eval(D(f,"x"))
  xt <- x
  if(d<eps){
    return(xt)
    break
  }
  x <- xt-d/df
  }
}
newton.raphson(0.5,f,eps=1e-10)
newton.raphson(0.6,f,eps=1e-10)

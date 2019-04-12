Newton.raphson <- function(f,x,o,eps=1e-10){
  x <- o[1]
  y <- o[2]
  rs <- eval(f)
  
    while(rs>eps){
      gradient <- matrix(attributes(eval(deriv(f,c("x","y"))))$gradient,byrow = F, 2,1)
      hessian <- matrix(attributes(eval(deriv(f,c("x","y"))))$hessian,byrow = F, 2,2)
      print(attributes(eval(deriv(f,c("x","y"))))$hessian)
      nxy <- hessian %*% gradient
      x <- nxy[1]
      y <- nxy[2]
      rs <- eval(f)
    }
  return(c(x,y))
}

f <- expression(x^2-x*y+y^2+exp(y))
Newton.raphson(f,c(x,y),c(1,1))
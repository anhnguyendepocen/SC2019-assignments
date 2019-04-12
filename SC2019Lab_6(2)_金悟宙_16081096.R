Newton.raphson <- function(f,o,eps=1e-10){
  nxy <- matrix(o)
  x <- o[1]
  y <- o[2]
  rs <- c(1,0)#任意指定两个值，保存两次的函数值#
  
  while(abs(rs[2] - rs[1])>eps){
    he <- eval(deriv(f,c("x","y"), hessian = T))#deriv#
    gradient <- matrix(attributes(he)$gradient,byrow = F, 2,1)#梯度矩阵#
    hessian <- matrix(attributes(he)$hessian,byrow = F, 2,2)#海塞矩阵#

    nxy <- nxy - solve(hessian, gradient)
    x <- nxy[1]
    y <- nxy[2]
    rs[1] <- rs[2]  
    rs[2] <- eval(f)
  }
  
  return(c(x,y))
}

f <- expression(x^2-x*y+y^2+exp(y))
Newton.raphson(f,c(1,1))
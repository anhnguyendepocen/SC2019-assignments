findroot <- function(f, x0, i, d = NULL){
  if(is.null(d))(d <- D(f, "x"))
  k <- 10
  x <- x0
  while(k > i){
    x <- x - eval(f)/eval(d)
    k <- eval(f)
  }
  return(x)
}

f <- expression(y = x^2 - 5)
findroot(f, 2, 0.00001)

f <- expression(y = sqrt(abs(x)))
d <- expression(sign(x)*(0.5 * abs(x)^-0.5)) #abs() no derivation, setting advance
findroot(f, 0.25, 0.00001, d)
findroot(f, 0.5, 0.00001, d)
#special curve, each x is the opposite of the previous, can't solve by newtown method
#Plot:sqrt(abs(x))
x = 4
curve(sqrt(abs(x)), -10, 10)
abline(1, eval(d), col = 2)
abline(1, -eval(d), col = 2)
abline(v = 4)
abline(v = -4)
abline(h = 0)

f <- expression(y = 0.4*(exp(x)+1)^-1 + 0.2 - x*exp(-x^2))
findroot(f, 0.5, 0.00001)
findroot(f, 0.6, 0.00001)

#Some functions can't be solved by Newton method 

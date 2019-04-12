## 1.Write R code to find the root of x^2=5
#x0-initial value of x; f-function formula; e-the limit
newton.raphson <- function(f, x0, e){
  i = 1
  x <- x0
  while(i > 0){
    x = x - eval(f)/eval(D(f, "x"))
    if(abs(eval(f)) < e)
      break
    else
      i = i + 1
  }
  return(c(x,i))
}

f1 <- expression(x^2-5)
newton.raphson(f1, 2, 0.001)

## 2.Now use your Newton-Raphson code to find the root of g(x)=√|x|.Try initial value 0.25.
f2 <- expression(sqrt(sqrt(x^2)))
newton.raphson(f2, 0.25, 0.001)

f <- function(x){
  return(sqrt(abs(x)))
}
uniroot(f, c(0,2))

## 3.Now use your Newton-Raphson code to find the root of xe^(−x^2)=0.4(e^x+1)^(−1)+0.2. Try initial values 0.5 and 0.6.
f3 <- expression(x*e^(-x^2)-0.4/(e^x+1)-0.2)
newton.raphson(f3, 0.5, 0.001)
newton.raphson(f3, 0.6, 0.001)


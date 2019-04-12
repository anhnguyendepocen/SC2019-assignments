## Newton’s method

# Find the root of x^2 = 5
fun1 <- function(x) {return(x^2 - 5)} #Creating the function y = x^2 - 5
root1 <- uniroot(fun1, c(-3,0), tol = 1e-9)
root1$root #Get root -2.236068
root2 <- uniroot(fun1, c(0,3), tol = 1e-9)
root2$root #Get root 2.236068

# Create a function to calculate the root of all expressions using Newton-Raphson Method
newton.raphson <- function(expression, start, precise) {
  f1 <- as.expression(expression)
  f2 <- as.formula(paste("y~",expression))
  n <- 0
  xv <- c(start)
  while (abs(deriv(f2,"x",func = T)(xv[n+1])[1]) > precise) {
    fx <- deriv(f2,"x",func = T)(xv[n+1])[1]
    Dfx <- as.numeric(attr(deriv(f2,"x",func = T)(xv[n+1]),"gradient"))
    xv[n+2] <- xv[n+1] - fx / Dfx
    n <- n+1
    print(xv[length(xv)])
  }
  return(xv[length(xv)])
}

# Use Newton-Raphson code given to find the root of f(x)=sqrt(abs(x)). Try initial value 0.25.
newton.raphson("sqrt(sqrt(x^2))", 0.25, 10^(-9))
# Sink into an infinite loop with 0.25 & -0.25...

# Now use Newton-Raphson code to find the root of xe−x2=0.4(ex+1)−1+0.2. Try initial values 0.5 and 0.6.
# Try 0.5
newton.raphson("0.4*(exp(x)+1)^(-1)+0.2 - x*exp(-x^2)", 0.5, 10^(-9))
# Returned 0.4303877
# Try 0.6
newton.raphson("0.4*(exp(x)+1)^(-1)+0.2 - x*exp(-x^2)", 0.6, 10^(-9))
# Returned 0.4303877
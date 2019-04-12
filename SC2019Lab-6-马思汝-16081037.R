## Newton-Raphson Function
## f-the function which you want to calculate
## val_a & val_b-the initial value for tow variables
## e-the limit of the final reult
multi.newton.raphson <- function(f, val_a, val_b, e){
  i = 0
  x = c(val_a, val_b)
  df <- deriv(y, c("a", "b"), hessian = TRUE, func = TRUE)
  while(i >= 0){
    root <- c(val_a, val_b)# store the value
    re_df <- df(val_a, val_b)# calculate the corresponding matrix
    re_d1 <- matrix(attributes(re_df)$gradient, 1, 2)# get the gradient matrix
    re_d2 <- matrix(attributes(re_df)$hessian, 2, 2)# get the hessian matrix
    i = i + 1# number of calculations
    sol <- re_d1 %*% solve(re_d2) #calculate the step
    root = root - sol
    val_a <- root[1]# reassign variables
    val_b <- root[2]
    if(re_d1[1] < e & re_d1[2] < e){ # set up when to break
      break
    }
  }
  return(c(root, i))# return the result and the number of cycle
}

y <- expression(a^2-a*b+b^2+exp(b))
multi.newton.raphson(y, -1, 0, 0.001)



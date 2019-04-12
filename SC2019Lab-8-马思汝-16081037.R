## Lab-8
# 1.Find the minimum of the function f(x)=x1^2+x2^2
# 2.Use a triangle with vertices (1,1),(1,2),(2,2) as the starting simplex
# 3.Don't worry about using a loop just yet. Try to get code that just does the first iteration.
# 4.Don't worry about the stopping rule yet either

library(ggplot2)

# function to calculate one point
calpoint <- function(coordinate){
  ans <- coordinate[1]^2 + coordinate[2]^2
  return(ans)
}

# function to calculate each points
evalfunc <- function(value){
  evalf <- c()
  for (i in 1:length(value)) {
    evalf[i] <- value[[i]][1]^2 + value[[i]][2]^2
  }
  return(evalf)
}

# find centroid
findx0 <- function(value){
  x0 <- c(0, 0)
  for (i in 1:length(value)) {
    for (j in 1:length(x0)) {
      x0[i] = x0[i] + value[[j]][i]
    }
  }
  x0 <- x0 / length(x0)
  return(x0)
}

# set up the initial value
initial <- list(c(1,1), c(1,2), c(2,2))
value <- list(c(1,1), c(1,2), c(2,2))
# Nelder.Mead Functon
Nelder.Mead <- function(value){
  n = length(value)# number of points
  t = 1# count of time
  alpha = 1# set the value of alpha
  gama = 2# set the value of gama
  rou = 0.5# set the value of rou
  deta = 0.5# set the value of deta
  while(t < 100){
    ans <- evalfunc(value)# calculate the value off each point
    xmax <- value[[which(ans >= max(ans))]]# get the max point
    rexmax <- calpoint(xmax)# calculate the function at the max point
    newvalue <- value# elimate the worst point
    value[[which(ans >= max(ans))]] <- NULL
    x0 <- findx0(value)# find x0
    xr <- x0 + alpha * (x0 - xmax)# find xr
    rexr <- calpoint(xr)# re-calculate the value of new points
    rex1 <- calpoint(value[[1]])
    rex2 <- calpoint(value[[2]])
    if(rex1 <= rexr & rex2 >= rexr){
      value <- c(value, list(xr))
      t = t + 1
    }
    else if(rexr < rex1){
      xe <- x0 + gama * (xr - x0)# find expand point
      rexe <- calpoint(xe)
      if(rexe < rexr){
        value <- c(value, list(xe))
      }
      else if(rexe >= rexr){
        value <- c(value, list(xr))
      }
      t = t + 1
    }
    else if(rexr > rex2){
      xc = x0 + rou * (xmax - x0)# find contraction point
      rexc <- calpoint(xc)
      if(rexc < rexmax){
        value <- c(value, list(xc))
      }
      else{
        value <- newvalue
        for (i in 1:n) {
          value[[i]] = value[[1]] + deta * (value[[i]] - value[[1]])
        }
      }
      t = t + 1
    }
    result <- matrix(1, 2, n)
    for (k in 1:n) {
      result[, k] <- value[[k]] - value[[3]]
    }
    z <- matrix(c(rep(1, n)), nrow = 1)
    result <- rbind(result, z)
    X <- 1/2 * abs(det(result))
    if(X < 0.001){
      break
    }
  }
  return(c(t, X, value))
}
# test the function with initial value
Nelder.Mead(initial)





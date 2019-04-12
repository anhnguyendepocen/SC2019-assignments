# use The Power Method to redo google pagerank problem
# input the matrix A
A <- matrix(c(0,1,0,1,1,
              0,0,1,1,1,
              1,0,0,1,0,
              0,0,0,0,1,
              0,0,1,0,0), nrow = 5)
# power method function
Power.fun <- function(A, initialX, maxt){
  i=0
  x <- initialX
  q <- x / sqrt(sum(x^2))
  for (i in 1:maxt) {
    x <- A %*% q
    q <- x / sqrt(sum(x^2))
    lamda <- t(q) %*% A %*% q
    i = i + 1
  }
  return(list(lamda = lamda, X = x))
}
# set up initial matrix of X
X <- rep(1, nrow(A))
# test the function
result <- Power.fun(A, X, 1000)
order(result$X)

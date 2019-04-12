# Power Method to calculate eigenvalues & eigenvectors
myEigen <- function(A,iter=10000){ 
  n <- nrow(A)
  x <- rep(1,n) # generate initial vector x0
  q <- x/sqrt(sum(x^2)) # normalise x
  for (i in 1:iter) { # define iteration time=10000
    x <- A%*%q # compute next x
    q <- x/sqrt(sum(x^2)) # normalise next x
    lamda <- t(q)%*%A%*%q # estimate lamda
  }
  return(list(eigenvalues=lamda,vectors=x))
}


A <- matrix(c(0,1,0,1,1
              ,0,0,1,1,1
              ,1,0,0,1,0
              ,0,0,0,0,1
              ,0,0,1,0,0),nrow = 5 , byrow = TRUE)
e <- myEigen(t(A))
order(e[[2]],decreasing = TRUE)

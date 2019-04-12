QR.decomposition <- function(x,iter=100000){
  Vec <- diag(1,nrow(x))
  for (i in 1:iter) { # define iteration time=10000
    Q <- qr.Q(qr(x)) # QR decomposition x
    R <- qr.R(qr(x))
    x <- R%*%Q # compute next x
    Vec <- Vec%*%Q # compute eigenvectors of x

  }
  e <- diag(Q%*%R) # calculate eigenvalues
  v <- Vec%*%qr.Q(qr(x)) # calculate eigenvalues of next x
  return(list(eigenvalues=e,eigenvectors = v))
}
A <- matrix(c(1,2,3,4),nrow =2,byrow=TRUE)
b <- QR.decomposition(A)


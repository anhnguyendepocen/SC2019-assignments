# code up QR algorithm
qrfun <- function(X){
  # input a matrix X
  reQ <- diag(nrow(X))# set the initial matrix of eigenvector
  i = 0
  while (i >= 0){
    qrans <- qr(X)
    Q <- qr.Q(qrans)
    R <- qr.R(qrans)
    X <- R %*% Q # calculate the eigenvalue matrix
    reQ <- reQ %*% Q # calculate the eigenvector
    i = i + 1 # count the calculate times
    if(abs(X[lower.tri(X)]) < 1e-5){
      l.qrans <- qr(X)
      l.Q <- qr.Q(l.qrans) # calculate Q(k+1)
      l.reQ <- reQ %*% l.Q # get the final eigenvector
      break
    }
  }
  eigenvalue <- diag(X)
  return(list(times = i, eigenvalue = eigenvalue, eigenvector = l.reQ))
}
# test function with a matrix A
A <- matrix(c(1,3,2,4), nrow = 2)
qrfun(A)


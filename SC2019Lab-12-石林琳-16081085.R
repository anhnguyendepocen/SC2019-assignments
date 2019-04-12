#1.QR算法
QRalgorithm <- function(X,tol=1e-25,realvalue=T){
  A <- X
  while(abs(det(A)-det(diag(diag(A))))>tol){
    A <- qr.Q(qr(A)) %*% qr.R(qr(A))
    A <- qr.R(qr(A)) %*% qr.Q(qr(A))
  }
  if(realvalue){
    return(A)
  }else{
    A[lower.tri(A)] = 0
    return(A)
  }
}

A <- matrix(c(1,2,3,4),2,2,byrow = F)
QRalgorithm(A)
eigen(A)

#2.用广义逆解决线性规划问题，并与lm()结果比较

X<- matrix(c(2, 3, 4, 7, 5, 1, 8, -1, 3, -6, 7, 2), 4, 3)
y <- 5*X[,1] - 4*X[,2] + 1e-04 * (rnorm(4) - 0.5)
library(MASS)
re <- ginv(X[,1:2]) %*% y  
t(y - X[,1:2] %*% beta) %*% (y - X[,1:2] %*% re)

lmod <- lm(y~X[,1:2])
deviance(lmod)


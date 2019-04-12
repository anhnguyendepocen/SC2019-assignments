#generalised inverse & lm
library(MASS)
X <- matrix(c(1,1,1,1,2,5,8,9,4,6,9,10),4,3,byrow = T)
y <- 3*X[,2] + 4*X[,3] + 1e-04*rnorm(4)
beta_ginv <- ginv(X) %*% y
x1 <- X[,2]
x2 <- X[,3]
beta_lm <- lm(y~x1+x2)$coef
list('ginv' = beta_ginv, 'lm' = beta_lm)

#Power Method
power <- function(x0,A){
  n=0
  repeat{
    q0 = x0/sqrt(sum(x0*x0))
    x1 = A %*% q0
    q1 = x1/sqrt(sum(x1*x1))
    n = n+1
    if( sum(abs(q1-q0)) <= 1e-10 |n >= 100){
      eig_vec = q1
      eig_val = t(eig_vec) %*% A %*% eig_vec
      break
    }
    x0 = x1
  }
  l = list('eigen_value' = eig_val, 'eigen_vector' = eig_vec)
  return(l)
}
A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5,5)
a = power(c(1,1,1,1,1),A)
rank(a[[2]])


#QR 
qr_method <- function(A){
  n = 0
  repeat{
    A1 = qr.R(qr(A)) %*% qr.Q(qr(A))
    n = n+1
    if(sum(abs(A-A1)) < 1e-05 | n<=100) {
      return(A1)
      break
      }
    A = A1
  }
}
qr_method(matrix(c(1,3,2,4),2,2))  






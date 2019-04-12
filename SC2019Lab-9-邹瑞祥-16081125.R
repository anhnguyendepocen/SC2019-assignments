#L1 ginv
X <- matrix(rnorm(18,1,1),6,3)
y <- X%*%c(1,2,6)
library(MASS)
beta1 <- ginv(X) %*% y  # write your own function to find generalised inverse

myginv<-function(X){
  s <- svd(X)
  D <- diag(s$d)
  U<-s$u
  V<-s$v
  V%*%solve(D)%*%t(U)
}
beta2<-myginv(X)%*%y
lm(y~X)
#The above three method return the same result
#L2 rank the website
A = c(0, 1, 0, 1, 1,
      0, 0, 1, 1, 1,
      1, 0, 0, 1, 0,
      0, 0, 0, 0, 1,
      0, 0, 1, 0, 0)
m <- matrix(A, byrow = T, nrow = 5)

power<-function(A){
n<-ncol(A) 
x <- rep(1,n);
x<-x/sum(x)
for (i in 1:10){
  x <- A %*% x
  x<-x/sum(x)
} 
lamda<-t(x)%*%A%*%x
list(lamda=lamda,vector=x)
}
power(m)
x <- (eigen(t(m))$vectors)[, 1]
#[1] 0.2816479+0i 0.1697388+0i 0.4673389+0i 0.5536819+0i 0.6057177+0i
#The result of power method is same as eigen()
#L3
A=matrix(c(1,3,4,2),2,2)
QR<-function(A){
  A1=qr(A)
  Q=qr.Q(A1) 
  for (i in 1:100){
    Q1=qr.Q(A1) 
    R1=qr.R(A1)
    A1=R1 %*% Q1
    A1=qr(A1)
    Q=Q%*%qr.Q(A1)
  }
  result=list(Q,Q1 %*% R1,t(Q),Q%*%Q1 %*% R1%*%t(Q))
  result
}
QR(A)

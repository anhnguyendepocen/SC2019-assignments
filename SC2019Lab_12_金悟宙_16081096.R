#Use generalised inverse to solve a linear regression problem, and compare with the lm() function in R.

X <- matrix(c(10,7,11,17,30,19,1,8,5,6,16,3), 4, 3)
y <- 3 * X[, 1] - 5 * X[, 2] + 1e-04 * (rnorm(4) - 0.5)
library(MASS)
(beta <- ginv(X) %*% y)
      #              [,1]
      #[1,]  2.999987e+00
      #[2,] -5.000001e+00
      #[3,]  6.373987e-06
t(y - X %*% beta) %*% (y - X %*% beta)

x1 <- X[,1]
x2 <- X[,2]
x3 <- X[,3]
lm.model <- lm(y ~ x1+x2+x3)
summary(lm.model)
      #OK



#Now use The Power Method to redo your google pagerank problem

A <- matrix(c(0,1,0,1,1,
              0,0,1,1,1,
              1,0,0,1,0,
              0,0,0,0,1,
              0,0,1,0,0),5,5,byrow = TRUE)
A <- t(A)

stdx <- function(x){
  x <- x/sqrt(sum(x^2))
  return(x)
}

x0 <- matrix(data = c(1,2,3,4,5),5,1)
for(i in 1:1e+8){
  x1 <- stdx(x0)
  x0 <- A %*% x0
  x0 <- stdx(x0)
  if(sum(abs(x0 - x1)) < 1e-10){
    print(c(i,x0))
    break
    }
}
(x0 <- x0/sum(x0))



#Go to R code up QR algorithm.Use QR algorithm on A=(1,2;3,4)
A <- matrix(c(1,2,3,4),2,2,byrow = T)
eigen(A)
      #eigen() decomposition
      #$values
      #[1]  5.3722813 -0.3722813
      #
      #$vectors
      #[,1]       [,2]
      #[1,] -0.4159736 -0.8245648
      #[2,] -0.9093767  0.5657675
qr.m <- function(A){        #用于返回A的QR分解的Q矩阵和R矩阵
  Aqr <- qr(A)
  Q <- qr.Q(Aqr)
  R <- qr.R(Aqr)
  return(list(Q,R))
}

qrs <- function(A){         #QR算法，求上三角矩阵S
  for(i in 1:100){
    a <- qr.m(A)
    A <- a[[2]] %*% a[[1]]
  }
return(A)}
s <- qrs(A)
s
      #             [,1]       [,2]
      #[1,]  5.372281e+00 -1.0000000
      #[2,] 1.071841e-115 -0.3722813
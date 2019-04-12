# Statistical Computing Homework10
# author:Yuchao Tao   StudentID:16081045

rm(list = ls())
setwd("/Users/tyc_219/Desktop/data")
houseprice <- read.csv("maindata.csv", header = TRUE)
library(MASS)

#use the power method to solve pagerank problem
pagerank <- function(A, max.iter)
{
  i <- 0
  n <- nrow(A)
  x <- matrix(rep(1,n), nrow = n)
  while(i < max.iter){
    x <- A %*% x
    lambda <- max(x)
    x <- x/lambda
    i <- i + 1
  }
  x <- x/sum(x)
  out <- list(eigenvector_standered = x, eigenvalue = lambda)
  return(out)
}
A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)
rank <- pagerank(A,100)
sum(rank$eigenvector)

#QR composition
A <- matrix(c(1,2,3,4), 2, 2, byrow = T)
A
QR.composition <- function(A, max.loop = 100){
  i <- 0
  while(i < max.loop){
    B <- qr(A)
    Q <- qr.Q(B)
    R <- qr.R(B)
    S <- Q %*% R
    i <- i+1
    out <- list(S = S, Q = Q, R = R)
  }
  return(out)
}
QR.composition(A)

#For linear regression, compare svd, pseudo-inverse and QR decomposition in R
x <- as.matrix(cbind(rep(1,18),houseprice[,2:10])) #beta0：the corresponding x values are all 1
y <- as.matrix(houseprice[,1])
beta <- ginv(x) %*% y  
beta

result <- QR.composition(x)
betahat <- solve(result$R) %*% t(result$Q) %*% y
betahat

svd_com <- svd(x)
svd_com$d <- diag(svd_com$d)
betahat_svd <- solve(svd_com$v %*% (svd_com$d) %*% (svd_com$d) %*% t(svd_com$v)) %*% t(x) %*% y
betahat_svd

fits <- lm(P ~ ., data = houseprice)
summary(fits)

#these methods get the same estimate of betahat
list(lm = fits$coefficients, svd = betahat_svd, QR = betahat, ginv = beta)

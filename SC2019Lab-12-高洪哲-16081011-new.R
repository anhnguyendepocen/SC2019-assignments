setwd("D:/ProgramProject/R/Lab12")
library(MASS)
#generalised inverse
myginv <- function(x){
  m <- nrow(x)
  n <- ncol(x)
  r <- qr(x)$rank
  s <- svd(x)
  u <- s$u[,1:r]
  d <- s$d[1:r]
  v <- s$v[,1:r]

  ginv_x <- v%*%solve(diag(d))%*%t(u)
  return(ginv_x)
}
a <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40),4,3)
b <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40),3,4)

ginv(a)
myginv(a)
ginv(b)
myginv(b)


#QR algorithm & SVD
QRalgorithm <- function(matr,tol=1e-25,realvalue=T){
  A <- matr
  q <- diag(1,nrow(A))
  while(abs(det(A)-det(diag(diag(A))))>tol){
    qa <- qr(A)
    Q <- qr.Q(qa)
    R <- qr.R(qa)
    A <- qr.R(qr(A)) %*% qr.Q(qr(A))
    q <- q %*% Q
  }
  matrixQ = q
  evalue =sort(diag(A),decreasing = T)
  if(realvalue){
    return(list(matrix=A,evalue=evalue,matrixQ=q))
  }else{
    A[lower.tri(A)] = 0
    return(list(matrix=A,evalue=evalue,matrixQ=q))
  }
}
#my svd
mysvd <- function(mat){
  m <- nrow(mat)
  n <- ncol(mat)
  xxt <- mat%*%t(mat)
  xtx <- t(mat)%*%mat
  ru <- qr(xxt)$rank
  rv <- qr(xtx)$rank
  u <- QRalgorithm(xxt)$matrixQ[,1:ru]
  if(m>=n){
    d <- diag(sqrt(QRalgorithm(xtx)$evalue))
  }
  if(m<n){
    d <- diag(sqrt(QRalgorithm(xxt)$evalue))
  }
  v <- QRalgorithm(xtx)$matrixQ[,1:rv]
  return(list(u=u,d=d,v=v))
}

A=matrix(c(0,1,1,1,1,0),3,2)
mysvd(A)
mysvd(A)$u%*%mysvd(A)$d%*%mysvd(A)$v
svd(A)

#For linear regression, compare svd, pseudo-inverse and QR decomposition in R.
set.seed(1)
X <- matrix(c(1,1,1,1,1, 2, 4, 8, 3, 6, 9, 12), 4, 3)
y <- 2 * X[, 2] - 7 * X[, 3] + 1e-04 * (rnorm(4) - 0.5)*X[,1]
#1 lmod
lmod <- lm(y~X[,2:3])
beta_lm <- lmod$coefficients
beta_lm
#2 svd
s1 <- svd(X)
s2 <- mysvd(X)
beta_svd1 <- s1$v%*%solve(diag(s1$d))%*%t(s1$u)%*%y
beta_svd2 <- s2$v%*%solve(s2$d)%*%t(s2$u)%*%y
#3generalised inverse
beta_ginv1 <- ginv(X)%*%y
beta_ginv2 <- myginv(X)%*%y
#4QR
beta_qr <- 
result <- matrix(c(as.vector(beta_lm),
                   as.vector(beta_svd1),as.vector(beta_svd2),
                   as.vector(beta_ginv1),as.vector(beta_ginv2)),
                 ncol = 3,byrow = T)
colnames(result) <- c("beta0","beta1","beta2")
rownames(result) <- c("lm","svd","mysvd","ginv","myginv")
result
#经检查发现计算svd时u的方向存在差异
s1$u
s2$u

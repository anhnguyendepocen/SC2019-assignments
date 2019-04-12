setwd("D:/ProgramProject/R/Lab12")
#1.use The Power Method to redo your google pagerank problem
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),nrow = 5)
AT <- t(A)
pm <- function(A,n=9000){
  counts = 0
  x <- A[1,]
  q <- x/sum(x^2)
  while(counts<n){
    x <- A %*% q
    q <- x/sum(x^2)
    counts =counts +1
  }
  return(list(lambda=t(q)%*%A%*%q,ev=x))
}
eigen(AT)
result <- pm(AT)
alpha  <- 1/result$lambda
r <- result$ev/sum(result$ev)
#2.code up QR algorithm
QRalgorithm <- function(matr,tol=1e-25,realvalue=T){
  A <- matr
  while(abs(det(A)-det(diag(diag(A))))>tol){
    A <- qr.Q(qr(A)) %*% qr.R(qr(A))
    A <- qr.R(qr(A)) %*% qr.Q(qr(A))
  }
  evalue =sort(diag(A),decreasing = T)
  if(realvalue){
    return(list(matrix=A,evalue=evalue))
  }else{
    A[lower.tri(A)] = 0
    return(list(matrix=A,evalue=evalue))
  }
}

A <- matrix(c(1,2,3,4),2,2,byrow = F)

QRalgorithm(A)
eigen(A)
QRalgorithm(A,realvalue = F)
#3
#Use QR decomposition to write your own svd function in R.

mysvd <- function(mat){
  m <- nrow(mat)
  n <- ncol(mat)
  xtx <- t(mat)%*%mat
  xxt <- mat%*%t(mat)
  u <- eigen(xxt)$vectors
  if(m>n){
    p <- matrix(c(rep(0,(m-n)*n)),ncol = n)
    q <- diag(sqrt(QRalgorithm(xtx)$evalue))
    d <- rbind(q,p)
  }
  if(m<n){
    p <- matrix(c(rep(0,(n-m)*m)),nrow = m)
    q <- diag(sqrt(QRalgorithm(xxt)$evalue))
    d <- cbind(q,p)
  }
  if(m==n){
    d <- diag(sqrt(QRalgorithm(xtx)$evalue))
  }
  v <- eigen(xtx)$vectors
  return(list(u=u,d=d,v=v,p=p))
}
mysvd(A)
A=matrix(c(0,1,1,1,1,0),3,2)
svd(A)
mysvd(A)
mysvd(A)$u%*%mysvd(A)$d%*%mysvd(A)$v
svd(A)
#For linear regression, compare svd, pseudo-inverse and QR decomposition in R.

X <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40), 4, 
            3)
y <- 2 * X[, 1] - 7 * X[, 2] + 1e-04 * (rnorm(4) - 0.5)
library(MASS)
beta <- ginv(X[,1:2]) %*% y  # write your own function to find generalised inverse
t(y - X[,1:2] %*% beta) %*% (y - X[,1:2] %*% beta)
beta

lmod <- lm(y~X[,1:2])
deviance(lmod)



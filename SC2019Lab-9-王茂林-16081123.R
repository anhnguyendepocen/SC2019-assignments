############
####ginv####
############
library(MASS)
gginv<- function(A){
  svd_A<-svd(A)
  u<-svd_A$u
  v<-svd_A$v
  d<-diag(svd_A$d,nrow = ncol(u),ncol = nrow(v))
  a<-u%*%solve(d)
  result <- a%*%t(v)
  t(result)
}

x<-matrix(c(-5:5),ncol = 1)
y<-1+x%*%c(1)+rnorm(11)
beta <- gginv(x) %*% y
beta
lm(y~x)

x<-matrix(c(-5:5,-5:5),ncol = 2)
y<-1+x%*%c(1,1)+rnorm(11)
beta <- gginv(x) %*% y
beta
beta <- ginv(x) %*% y
beta
lm(y~x)#because of the multicollinearity lm(),gginv() can't estimate the beta, but ginv() can.

x<-matrix(c(-5:5,1,4,6,3,7,9,1,5,0,4,3),ncol = 2)
y<-1+x%*%c(1,1)+rnorm(11)
beta <- gginv(x) %*% y
beta
lm(y~x)

##############
#Power Method#
##############
power<- function(A,epsilon = 1e-6){
  n<-ncol(A)
  x<-rep(1,n)/n
  delta<-1e+10
  while(delta >= epsilon){
  delta <- x
  x<-A%*%x
  x<-x/sum(x)
  delta<- sum(abs(x-delta))
  }
  x
}
A= c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0)
m <- matrix(A,nrow = 5)
power(t(m))
#function eigen()
x <- (eigen(t(m))$vectors)[, 1]
p <- x/sum(x)
p
#result is same as power method: rank 5 4 3 1 2

######
##QR##
######
A <- matrix(c(1,3,2,4),nrow=2)
A
for(i in 1:100){
  qr_A<-qr(A)
  A<- qr.R(qr_A)%*%qr.Q(qr_A)
  print(A)
}
eigen(A)
#result is same as QR method
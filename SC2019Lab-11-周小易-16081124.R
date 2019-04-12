#lab-11
#QR算法
A<-matrix(c(1,3,2,4),2)

qral<-function(x,n=10){
  a<-x
  k<-0
  nr<-nrow(x)
  nc<-ncol(x)
  
  while (k<n) {
    qrresult<-qr(a)
    a1<-qr.R(qrresult) %*% qr.Q(qrresult)
    a<-a1
    k<-k+1
    if(k==1) Q<-qr.Q(qrresult)
    else Q<-qr.Q(qrresult) %*% Q
  }
  e<-diag(a)
  
  return(list(eigen=e,Q))
}
(e<-qral(A))
(a<-eigen(A))


#用伪逆求解线性回归beta
x<- c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40)
y <- 2 * x + 1e-1 * rnorm(12)
y
library(MASS)
beta <- ginv(x) %*% y
lm(y~x)

#利用幂法进行网页排序
A<-matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),5)

At<-t(A)
x <- rep(1,5)

for (i in 1:200) {
  y<-x/sqrt(sum(x^2))
  x<-At %*% y
  
  if(i == 100){
    e<-t(y) %*% A %*% y
  }
}
x
e


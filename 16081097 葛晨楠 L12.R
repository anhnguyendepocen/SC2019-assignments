A <- matrix(c(1,3,2,4),ncol = 2)
A

QRMethod <- function(matrixA,maxtime = 1000)
  {
  n <- 0
  qrresult <- qr(matrixA)
  qrq <- qr.Q(qrresult)
  qrr <- qr.R(qrresult)
  while(n <= maxtime)
  {
    matrixA <- qrr %*% qrq
    qrresult <- qr(matrixA)
    qrq <- qr.Q(qrresult)
    qrr <- qr.R(qrresult)
    n <- n+1
  }
  print(matrixA)
}
QRMethod(A,1000)


a <- c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0)
A <- matrix(a, byrow=T, nrow=5)
At <- t(m)
At

xk <- c(0,0,0,0,1)
qk <- (xk-mean(xk))/(sd(xk))

for(i in 1:1000)
{
  xk <- At %*% qk
  qk <- (xk-mean(xk))/(sd(xk))
  lendak <- t(qk) %*% At %*% qk
}
qk
xk
lendak

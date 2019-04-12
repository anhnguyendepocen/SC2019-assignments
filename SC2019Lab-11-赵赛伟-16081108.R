####SC2019Lab-11-赵赛伟-16081108####
svd_fun <- function(A){
  dima <- dim(A)
  dima_min <- min(dima)
  B <- t(A) %*% A
  C <- A %*% t(A)
  b <- eigen(B)
  c <- eigen(C)
  dimb <- dim(b$vectors)
  dimc <- dim(c$vectors)
  d <- matrix(0,dima[1],dimb[2])
  for(i in 1:dima_min){
    if(c$values[i] != 0){
      d[i,i] <- (c$values[i]) ^ 0.5
    }
  }
  dimd <- dim(d)
  print("$d")
  print(diag(d))
  print("$u")
  print(c$vectors[1:dima[1],1:dima_min])
  print("$v")
  print(b$vectors[1:dima[2],1:dima_min])
}

##其中，通过QR分解计算：
##t(A) * A和A * t(A)
##A = QR t(A) = t(R)*t(Q)
qra <- qr(A)
Q <- qr.Q(qra)
R <- qr.R(qra)
B <- t(R) %*% t(Q) %*% Q %*% R
C <- Q %*% R %*% t(R) %*% t(Q)

##通过QR分解计算奇异值
qrafun <- function(A){
  count <- 0
  dima <-  dim(A)
  dima_min <- min(dima[1],dima[2])
  while(A[dima_min,1] > 1e-05){
    QRA <- qr(A)
    Q <- qr.Q(QRA)
    R <- qr.R(QRA)
    Q <- Q[1:dima_min,1:dima_min]
    R <- R[1:dima_min,1:dima_min]
    A <- R %*% Q
    count <- count + 1
  }
  
  return(diag(A))
}

##于是得到应用QR分解的svd函数为
qra_svd_fun <- function(A){
  dima <- dim(A)
  dima_min <- min(dima)
  ##计算t(A) * A和A * t(A)
  qra <- qr(A)
  Q <- qr.Q(qra)
  R <- qr.R(qra)
  B <- t(R) %*% t(Q) %*% Q %*% R
  C <- Q %*% R %*% t(R) %*% t(Q)
  
  ##计算特征值
  b <- qrafun(B)
  c <- qrafun(c)
  
  
  b <- eigen(B)
  c <- eigen(C)
  dimb <- dim(b$vectors)
  dimc <- dim(c$vectors)
  d <- matrix(0,dima[1],dimb[2])
  for(i in 1:dima_min){
    if(c$values[i] != 0){
      d[i,i] <- (c$values[i]) ^ 0.5
    }
  }
  dimd <- dim(d)
  print("$d")
  print(diag(d))
  print("$u")
  print(c$vectors[1:dima[1],1:dima_min])
  print("$v")
  print(b$vectors[1:dima[2],1:dima_min])
}

A <- matrix(c(1,3,2,4),2,2)
qra_svd_fun(A)
svd_fun(A)
svd(A)

##自编svd函数与R语言自带svd函数进行对比
##存在一些正负号区别，但是因为负号可以提出矩阵抵消，所以本质上一样

##不同类型矩阵试验
B <- matrix(c(1,2,3,4,5,6,7,8),2,4)
qra_svd_fun(B)
svd_fun(B)
svd(B)

c <- matrix(c(1,2,3,4,5,6,7,8),4,2)
qra_svd_fun(c)
svd_fun(c)
svd(c)

##线性回归的svd，伪逆分解，QR分解比较
##线性回归
lm_test <- function(x,y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(dim(x)[1] == dim(y)[1]){
    beta <- ginv(x) %*% y  #get beta
    eta <- t(y - x %*% beta) %*% (y - x %*% beta) #get Residual value
    print(beta)
    print(paste("eta is",eta))
  }
  else{
    print("someting wrong with the data")
  }
}

##根据函数可知，我们计算的基本理论都是伪逆分解
##而  QR分解可以写成svd分解
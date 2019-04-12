####SC2019Lab-10-赵赛伟-16081108####
####Power Method to redo your google pagerank problem
A <- matrix(c(0,0,1,0,0,
              1,0,0,0,0,
              0,1,0,0,1,
              1,1,1,0,0,
              1,1,0,1,0),5,5)

x0 <- c(1,2,3,4,5)
xk <- A^10000 %*% x0
xk
##根据重要度结果，得排名为2,1,3,4,5 (与我们之前计算结果是一样的)

####QR decomposition
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
  print("特征值为：")
  print(diag(A))
}

B <- matrix(c(1,3,2,4),2,2)
qrafun(B)

C <- matrix(c(2,3,4,3,2,6,2,7,3,1,2,34),3,4)
qrafun(C)

D <- matrix(c(3,2,4,6,2,5,7,22,5,6,33,6),4,3)
qrafun(D)
##其他各种矩阵类型（高矩阵等）都可以
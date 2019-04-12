##############PageRank############
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),5)

#eigen
A.eigen <- eigen(A)
A.vector <- A.eigen$vectors[,1]
rank(A.vector/sum(A.vector))

#power method
n <- nrow(A)
b <- rep(1/n,n)
for(i in 1:1000){
  b <- A%*%b
  b <- b/c(sqrt(t(b)%*%b))
}

b - A.vector #the result is same with function eigen()
A.vector <- b
A.values <- (t(A.vector)%*%A%*%A.vector)/c(sqrt(t(A.vector)%*%A.vector))
A.values
A.eigen$values[1]


#########QR decomposition####
B <- matrix(c(1,3,2,4),2)
eigen(B)
tmp <- B
for(i in 1:1000){
  tmp.qr <- qr(tmp)
  Q <- qr.Q(tmp.qr)
  R <- qr.R(tmp.qr)
  tmp <- R%*%Q
}
B.values <- diag(tmp)



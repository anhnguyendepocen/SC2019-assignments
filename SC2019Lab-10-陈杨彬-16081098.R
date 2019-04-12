#第十一讲
#Use generalised inverse to solve a linear regression problem, 
#and compare with the lm() function in R.

#这是老师课件上用广义逆求解线性回归
X <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40), 4, 3)
y <- 2 * X[, 1] - 7 * X[, 2] + 1e-04 * (rnorm(4) - 0.5)
library(MASS)
beta <- ginv(X) %*% y
beta
      #[,1]
      #[1,]  0.3334003
      #[2,] -0.3333210
      #[3,]  1.6666842
t(y - X %*% beta) %*% (y - X %*% beta)
#接下来用lm()
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
lm.model<-lm(y~x1+x2+x3)
summary(lm.model)
#lm()拟合结果符合y与x的关系，广义逆的结果我认为是错的，y与x1,x2,x3
#的关系从y <- 2 * X[, 1] - 7 * X[, 2] + 1e-04 * (rnorm(4) - 0.5)就可以
#看出来beta应该是[Intercept,2,-7]



#第十二讲作业
#Now use The Power Method to redo your google pagerank problem
A=matrix(c(0,1,0,1,1,
           0,0,1,1,1,
           1,0,0,1,0,
           0,0,0,0,1,
           0,0,1,0,0),5,5,byrow = TRUE)
A<-t(A)
detx<-function(x){#将向量标准化
  x<-x/sqrt(sum(x^2))
  return(x)
}
x0<-matrix(data = c(10,11,100,1,-1),5,1)
for(i in 1:1e+8){#The Power Method
  x1<-detx(x0)
  x0<-A%*%x0
  x0<-detx(x0)
  if(sum(abs(x0-x1))<1e-10){print(c(i,x0));break}
}
(x0<-x0/sum(x0))

#Go to R code up QR algorithm.
#Use QR algorithm on A=(1,2,
#                       3,4)
A<-matrix(c(1,2,3,4),2,2,byrow = T)
eigen(A)
    #$values
    #[1]  5.3722813 -0.3722813
qr_A<-function(A){#用于返回A的QR分解的Q矩阵和R矩阵
  qrA<-qr(A)
  Q<-qr.Q(qrA)#Q是正交矩阵
  R<-qr.R(qrA)
  return(list(Q,R))
}

qrs<-function(A){#QR算法，求上三角矩阵S
  for(i in 1:100){
    a<-qr_A(A)
    A<-a[[2]]%*%a[[1]]
  }
return(A)}
s<-qrs(A)
s
    #[,1]       [,2]
    #[1,]  5.372281e+00 -1.0000000
    #[2,] 1.071841e-115 -0.3722813  对角线元素即为特征值，与eigen()结果一致
# 1.Use generalised inverse to solve a linear regression problem, and compare with the lm() function in R.
library(readxl)
library(MASS)
dat <- read_excel("C:/Users/25070/Desktop/RÓïÑÔ/Code/CH-02/MacEcoData.xlsx",sheet = "data1")
dat.log <- log(dat)
dim(dat.log)
X <- as.matrix(dat.log[,2:4])
X.inv <- cbind(X,rep(1,132))
Y <- as.matrix(dat.log[,1])
## estimate beta by lm() function generalised inverse method
model.lm <- lm(M2~.,data = dat.log)
summary(model.lm)
## estimate beta by 
beta <- ginv(X.inv)%*%Y
## write a function to calculate general inverse and estimate beta
General.inverse <- function (X) { 
  Xsvd <- svd(X) # calculate singular values,matrix u and v 
  d.inv <- 1/Xsvd$d
  g.inv <- Xsvd$v %*% (d.inv * t(Xsvd$u))
  return(g.inv)
}
g.inv <-General.inverse(X.inv)
beta <- g.inv%*%Y
beta

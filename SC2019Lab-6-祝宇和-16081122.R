# 求值函数
solv <- function(express, factors, values) {
  if (length(factors) != length(values)) {return(NULL)}
  for (i in 1:length(factors)) {
    assign(factors[i], values[i])
  }
  solution <- eval(express)
  return(solution)
}

# 表达式求导生成Hessian矩阵的函数
auto.hessian <- function(express, factors, values) {
  if (length(factors) != length(values)) {return(NULL)}
  hessian <- matrix(nrow = length(factors), ncol = length(factors), byrow = T)
  for (i in 1:length(factors)) {
    exp1 <- D(express, factors[i])
    for (j in 1:length(factors)) {
      hessian[i,j] <- solv(D(exp1, factors[j]),factors,values)
    }
  }
  return(hessian)
}

# 生成各项一阶导的n*1矩阵的函数
dev1 <- function(express, factors, values) {
  if (length(factors) != length(values)) {return(NULL)}
  devmat <- matrix(nrow = length(factors), ncol = 1)
  for (i in 1:length(factors)) {
    devmat[i,1] <- solv(D(express, factors[i]), factors, values)
  }
  return(devmat)
}

# 主函数，求多元函数的极值
newton.raphson.mult <- function(express, factors, minstart=-10, maxstart=10, precise = 10^(-9)) {
  randstart <- NULL # 用列表保存随机初始值
  for (i in 1:100) {
    randstart[[i]] <- matrix(runif(length(factors), minstart, maxstart),length(factors),1)
  } #生成100组随机变量赋值
  result <- NULL #用列表保存100组迭代结果
  
  for (j in 1:100) {
    runi <- NULL #保存每一组初始值的迭代过程
    runi[[1]] <- randstart[[j]]
    dx <- dev1(express, factors, as.vector(randstart[[j]])) #生成初始一阶导矩阵
    hessian <- auto.hessian(express, factors, as.vector(randstart[[j]])) #生成Hessian矩阵
    k <- 1
    while (k < 20) {
      runi[[k+1]]<- runi[[k]] - solve(hessian) %*% dx
      dx <- dev1(express, factors, as.vector(runi[[k+1]])) #用于下一次迭代的一阶导
      hessian <- auto.hessian(express, factors, as.vector(runi[[k+1]])) #用于下一次迭代的Hessian
      k <- k+1
    }
    result[[j]] <- runi[[k]] #最终迭代结果进入列表
  }
  
  result2 <- result[duplicated(result) == F] #删除重复值
  return(result2)
}

# 对多元线性回归进行极大似然法参数估计
library(dplyr)
library(plyr)
max.likelihood.lm <- function(dataframe, minstart=-10, maxstart=10, precise = 10^(-9)) {
  datamat <- as.matrix(dataframe)
  factornum <- ncol(datamat) #变量数
  y <- as.matrix(datamat[,1],nrow(datamat),1) # 因变量矩阵
  x <- matrix(nrow = nrow(datamat), ncol = ncol(datamat))
  x[,1] <- rep(1, nrow(datamat))
  x[,2:ncol(datamat)] <- datamat[,-1] # 自变量矩阵
  
  factors <- vector()
  for (i in 1:ncol(datamat)) {
    factors[i] <- paste("B",i-1, sep = "")
  } #生成待估参数的参数名称
  
  expresschar <- ""
  for (j in 1:nrow(datamat)) {
    expresschar <- paste(expresschar, "(",y[j],"-","B0",sep = "")
    for (k in 2:ncol(x)) {
      expresschar <- paste(expresschar, "-", x[j,k], "*B", k-1, sep = "")
    }
    expresschar <- paste(expresschar, ")^2 +", sep = "")
  }
  expresschar <- substr(expresschar,1,nchar(expresschar)-1) #生成表达式的字符串形式
  expressquote <- as.quoted(expresschar)
  express <- as.expression(expressquote[[1]]) #生成待求的表达式
  
  newton.raphson.mult(express, factors)
}
# Output Result:
#[[1]]
#[,1]
#[1,] 1.862652e+03
#[2,] 1.420988e-03
#
#[[2]]
#[,1]
#[1,] 1.862652e+03
#[2,] 1.420988e-03

# Logistic回归的参数估计
max.likelihood.log <- function(dataframe, minstart=-10, maxstart=10, precise = 10^(-9)) {
  datamat <- as.matrix(dataframe)
  factornum <- ncol(datamat) #变量数
  y <- as.matrix(datamat[,1],nrow(datamat),1) # 因变量矩阵，由0/1构成
  x <- matrix(nrow = nrow(datamat), ncol = ncol(datamat))
  x[,1] <- rep(1, nrow(datamat))
  x[,2:ncol(datamat)] <- datamat[,-1] # 自变量矩阵
  
  factors <- vector()
  for (i in 1:ncol(datamat)) {
    factors[i] <- paste("B",i-1, sep = "")
  } #生成待估参数的参数名称
  
  expresschar <- ""
  for (j in 1:nrow(datamat)) {
    expresschar <- paste(expresschar, y[j],"* (B0",sep = "")
    for (k in 2:ncol(x)) {
      expresschar <- paste(expresschar, "+", x[j,k], "*B", k-1, sep = "")
    }
    expresschar <- paste(expresschar,") +", sep = "")
  }
  expresschar <- substr(expresschar,1,nchar(expresschar)-1) # 生成yini
  
  for (j in 1:nrow(datamat)) {
    expresschar <- paste(expresschar, "- log(1 + exp(B0",sep = "")
    for (k in 2:ncol(x)) {
      expresschar <- paste(expresschar, "+", x[j,k], "*B", k-1, sep = "")
    }
    expresschar <- paste(expresschar,"))", sep = "")
  } # 生成log项
  
  expressquote <- as.quoted(expresschar)
  express <- as.expression(expressquote[[1]]) #生成待求的表达式
  
  newton.raphson.mult(express, factors)
}

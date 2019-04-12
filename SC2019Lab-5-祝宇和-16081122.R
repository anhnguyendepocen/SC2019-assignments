## 二元函数Newton Raphson方法
# 输入值：函数表达式expression、函数中的两个参数、两个初始值、精确值
newton.raphson.duo <- function(f, factors, start1, start2, precise = 10^(-9)) {
  # 分别进行一阶、二阶导数
  f1 <- D(f, factors[1])
  f2 <- D(f, factors[2])
  f11 <- D(f1, factors[1])
  f12 <- D(f1, factors[2])
  f21 <- D(f2, factors[1])
  f22 <- D(f2, factors[2])
  n <- 0
  xv <- list(start) #用于存储包括初始值在内的所有迭代结果
  # 函数求值
  solute <- function(f, x1, x2) {
    return(eval(f))
  }
  # 开始迭代
  while (solute(f,xv[[length(xv)]][1],xv[[length(xv)]][2]) > precise) {
    solute.vector <- c(solute(f11,xv[[length(xv)]][1],xv[[length(xv)]][2]),solute(f21,xv[[length(xv)]][1],xv[[length(xv)]][2]),solute(f12,xv[[length(xv)]][1],xv[[length(xv)]][2]),solute(f22,xv[[length(xv)]][1],xv[[length(xv)]][2]))
    dxy2 <- solve(matrix(solute.vector,2,2))
    dxy <- matrix(c(solute(f1,xv[[length(xv)]][1],xv[[length(xv)]][2]),solute(f2,xv[[length(xv)]][1],xv[[length(xv)]][2])),2,1)
    matmult <- (dxy2) %*% (dxy)
    xv[[n+2]] <- matmult[1:2]
    n <- n+1
    print(matmult[1:2])
  }
  return(matmult[1:2])
}
newton.raphson.duo(expression(x1^2 - x1*x2 + x2^2 + exp(x2)),c("x1","x2"),0,0)
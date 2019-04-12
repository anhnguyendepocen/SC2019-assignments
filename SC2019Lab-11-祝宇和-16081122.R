## Linear Regression Using Generalized Inverse

library(faraway)
# 编写主函数，利用伪逆法进行线性模型的参数估计
# 要求输入一个包含样本值的数据框，其中第一列必须是因变量
lm.ginv <- function(dataframe) {
  datamat <- as.matrix(dataframe)
  factornum <- ncol(datamat) # 统计样本的变量数
  y <- as.matrix(datamat[,1],nrow(datamat),1) # 因变量矩阵y
  x <- matrix(nrow = nrow(datamat), ncol = ncol(datamat))
  x[,1] <- rep(1, nrow(datamat))
  x[,2:ncol(datamat)] <- datamat[,-1] # 自变量矩阵X
  
  # 开始对X进行奇异值分解
  rankx <- qr(x) $ rank #确定X的秩，即为保留奇异值的个数
  svdx <- svd(x) #对X矩阵进行奇异值分解
  UT <- (t(svdx $ u))[1:rankx,] #将U矩阵转置，并保留前几行
  sigma <- diag((svdx $ d)[1:rankx]) #保存需保留的奇异值，进入对角阵
  V <- (svdx $ v)[,1:rankx] #V矩阵保留前几列
  
  # 求出X矩阵的伪逆
  Xinv <- V %*% solve(sigma) %*% UT #公式：A+ = V1 * D(-1) * U1T
  
  # 根据B = X+ * y，求出系数矩阵B
  B <- Xinv %*% y
  return(B)
}

# 测试函数
setwd("D://BUAA//大三下//统计计算 康雁飞")
data0 <- read.csv("LinearTest.csv")
dataframe <- data0
lm.ginv(dataframe)
# Results
# [1,] 83.230092
# [2,]  2.290184
# [3,]  1.300989

# 将测试结果与lm函数相对比
lmt <- lm(data0[,1] ~ data0[,2] + data0[,3])
sumary(lmt)
# Results
# Estimate Std. Error t value  Pr(>|t|)
# (Intercept) 83.23009    1.57387 52.8825 4.572e-08
# data0[, 2]   2.29018    0.30406  7.5319 0.0006532
# data0[, 3]   1.30099    0.32070  4.0567 0.0097608
# 得出的结果与伪逆方法的结果一致
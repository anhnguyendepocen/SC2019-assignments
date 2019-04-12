power.method <- function(mat, times = 1000) {
  if (nrow(mat) != ncol(mat)) {
    cat("未输入有效方阵")
    return(NULL)
  }
  else{
    n <- nrow(mat) # 矩阵的维数
    x <- NULL # x是用来保存迭代向量xk的列表
    x[[1]] <- rep(1,n) # 列表中的第一个元素是x0
    q <- NULL # q是用来保存方向向量qk的列表
    q[[1]] <- x[[1]] / sqrt(sum((x[[1]])^2))
    # 将x0处以x0的模长，得到q0
    lambda <- vector() # 用来保存每次迭代产生的特征值
    
    # 开始power method迭代
    for (i in 1:times) {
      x[[i+1]] <- mat %*% q[[i]] # 该次迭代的xk
      q[[i+1]] <- x[[i+1]] / sqrt(sum((x[[i+1]])^2)) # 该次迭代的qk
      lambda[i] <- t(q[[i+1]]) %*% mat %*% q[[i+1]] # 该次迭代的特征值
    }
    
    # 用列表输出迭代结果
    result <- NULL
    result[[1]] <- x[[times+1]]
    result[[2]] <- lambda[length(lambda)]
    
    # 第一项是特征向量，第二项是特征值
    return(result)
  }
}

# 测试函数
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),5,5)
power.method(t(A))
# Result
# [[1]]
# [,1]
# [1,] 0.4673389
# [2,] 0.2816479
# [3,] 0.7754564
# [4,] 0.9187256
# [5,] 1.0050686
#
# [[2]]
# [1] 1.659302

# 可见，五个页面的影响力排名为4,5,3,2,1
# 第5个页面应该排在第一位
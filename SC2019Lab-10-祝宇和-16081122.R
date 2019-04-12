## Image Compression

library(jpeg)
setwd("D://BUAA//大三下//统计计算 康雁飞//pic.compose")

# 用于图像压缩的主函数，要求输入原始图像的文件名、希望压缩图保留的奇异值数量的序列
im.compress <- function(filename, ranks = c(286,200,116,32,10)) {
  tasknum <- length(ranks) #统计需要生成图像的数量
  
  mat0 <- readJPEG("origin.jpg") # 把图像变成了一个包含三个矩阵的三维数组，长度为2427*1698
  
  svd0 <- NULL
  for (i in 1:3) {
    svd0[[i]] <- svd(mat0[,,i])
    cat(paste("完成奇异值分解：第",i,"个矩阵\n",sep = ""))
  } # svd0用于保存mat0奇异值分解的结果
  
  # 对于每一个奇异值数量水平的压缩图，反解其原矩阵A并生成图像
  for (k in 1:tasknum) {
    compress0 <- array(dim = dim(mat0))
    for (j in 1:3) {
      U <- (svd0[[j]]$u)[,1:ranks[k]] #保留U矩阵的前几列
      sigma <- diag((svd0[[j]]$d)[1:ranks[k]]) #保存需保留的奇异值，进入对角阵
      VT <- (t(svd0[[j]]$v))[1:ranks[k],] #将V矩阵转置，并保留前几行
      compress0[,,j] <- U %*% sigma %*% VT #反解A
    }
    
    writeJPEG(compress0, paste("compression_", ranks[k], ".jpg", sep = "")) #保存图像
    cat(paste("生成图像：保留", ranks[k], "个奇异值\n", sep = ""))
  }
  cat("完成")
}
  
# 测试函数
im.compress("origin.jpg")

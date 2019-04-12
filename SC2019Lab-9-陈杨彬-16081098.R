library(jpeg)
library(rARPACK)#svds()函数，可只求前k个分解结果
setwd("G:/学习文件/任务/统计计算/作业/9")
factorize = function(m, k)#计算前k个分解结果
{
  r = svds(m[, , 1], k)#对三种颜色分别计算
  g = svds(m[, , 2], k)
  b = svds(m[, , 3], k)
  return(list(r = r, g = g, b = b))
}

recoverimg = function(lst, k){
recover0 = function(fac, k){
dmat = diag(k)#建k*k的对角阵
diag(dmat) = fac$d[1:k]#令对角阵的对角元素等于d的前k个对角元素
m = fac$u[, 1:k] %*% dmat %*% t(fac$v[, 1:k])#u[,1:k]行全取，列只取前k列
m[m < 0] = 0#矩阵内的值都在0，1之间，所以对大于1小于0的值要归并到0，1
m[m > 1] = 1
return(m)}
r = recover0(lst$r, k)
g = recover0(lst$g, k)
b = recover0(lst$b, k)
m = array(0, c(nrow(r), ncol(r), 3))#与r相同行列数但有三页的矩阵
m[, , 1] = r
m[, , 2] = g
m[, , 3] = b
return(m)}

picture = readJPEG("3.jpg")
#all(picture<=1)
#all(picture>=0)
lst = factorize(picture, 100)
k = c(1, 5, 20, 50, 100)
for(i in k){
  m = recoverimg(lst, i)
  writeJPEG(m, sprintf("svd_%d.jpg", i), 0.95)
}
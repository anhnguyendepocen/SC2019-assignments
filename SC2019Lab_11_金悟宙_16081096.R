library(jpeg)
library(rARPACK)
factorize = function(m, k)#计算前k个分解结果#
{
  r = svds(m[, , 1], k)
  g = svds(m[, , 2], k)
  b = svds(m[, , 3], k)
  return(list(r = r, g = g, b = b))
}

recoverimg = function(lst, k){#返回图像矩阵#
recover0 = function(fac, k){
dmat = diag(k)
diag(dmat) = fac$d[1:k]
m = fac$u[, 1:k] %*% dmat %*% t(fac$v[, 1:k])
m[m < 0] = 0
m[m > 1] = 1
return(m)}
r = recover0(lst$r, k)
g = recover0(lst$g, k)
b = recover0(lst$b, k)
m = array(0, c(nrow(r), ncol(r), 3))
m[, , 2] = g
m[, , 3] = b
return(m)}

picture = readJPEG("C:/Users/Ozu/Desktop/timg.jpg")
lst = factorize(picture, 100)
k = c(1, 5, 20, 50, 100)
for(i in k){
  m = recoverimg(lst, i)
  writeJPEG(m, sprintf("C:/Users/Ozu/Desktop/timg(1).jpg"), 0.95)
}
## PCA
head(USArrests)
summary(pc.cr <- princomp(USArrests, cor = TRUE))
## 每个主成分对方差的贡献比例，显然Comp.1 +  Comp2所占比例超过85%，因此能够用前两个主成分来表示整个数据集，也将数据从4维降到两维
## 接下来查看每个特征在主成分中所在的比例
loadings(pc.cr)
## 根据以上数据可很容易转换为几个数学等式：
# Comp1 = -0.536 * Murder + (-0.583) * Assault + (-0.278)*UrbanPop + (-0.543)* Rape
# Comp2 = 0.418 * Murder + 0.188 * Assault + (-0.873)*UrbanPop + (-0.167)* Rape
## 可以用Comp1、Comp2两个维度的数据来表示各州，在二维图上展现各州个聚类关系。
head(pc.cr$scores) ##scores包含有各州在四个主成分的得分
## 将前两个Comp提取出来，转换为data.frame方便会面绘图
stats.arrests <- data.frame(pc.cr$scores[, -c(3:4)])
head(stats.arrests)
library(ggplot2)
##展现各州的分布情况，观察哪些州比较异常，哪些能够进行聚类
ggplot(stats.arrests, aes(x = Comp.1, y = Comp.2)) + xlab("First Component") + ylab("Second Component") + geom_text(alpha = 0.75, label = rownames(stats.arrests), size = 4)



## SVD
## 载入图片，并且显示出来
library(raster)
library(jpeg)
raster.photo <- raster("C:/Users/THINK/Desktop/Rlogo.jpg")
photo.flip <- flip(raster.photo, direction = "y")
## 将数据转换为矩阵形式
photo.raster <- t(as.matrix(photo.flip))
dim(photo.raster)
image(photo.raster, col = grey(seq(0, 1, length = 256)))  ##灰化处理
## 奇异值分解
photo.svd <- svd(photo.raster)
d <- diag(photo.svd$d)
v <- photo.svd$v
u <- photo.svd$u
## 取第一个奇异值进行估计
u1 <- as.matrix(u[, 1])
d1 <- as.matrix(d[1, 1])
v1 <- as.matrix(v[, 1])
photo1 <- u1 %*% d1 %*% t(v)
image(photo1, col = grey(seq(0, 1, length = 256)))
## 取前50个奇异值进行估计
u2 <- as.matrix(u[, 1:50])
d2 <- as.matrix(d[1:50, 1:50])
v2 <- as.matrix(v[, 1:50])
photo2 <- u2 %*% d2 %*% t(v2)
image(photo2, col = grey(seq(0, 1, length = 256)))







###SC2019Lab_9-Su Feihu-16081041

# set path
setwd('D:/study/code and data for R in finance/CH-02')
rm(list = ls())                   

# read datas
library(nFactors)      # for the optimal number of factors
library(readxl)
dat <- read_excel(path='MicEcoData.xlsx', sheet = "factor")
head(dat)
tail(dat)
dat.fact <- dat[,-1]      # remove stock code
names(dat.fact) <- paste('x', 1:ncol(dat.fact), sep='')

##PCA
# determine the optimal number of factors
ev <- eigen(cor(dat.fact)) # get eigenvalues   计算相关系数矩阵的特征值  特征向量
ev
ap <- parallel(subject=nrow(dat.fact),var=ncol(dat.fact), rep=100, cent=.05) #平行分析
ap
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)    #确定最优因子数目
nS

library(psy)
scree.plot(dat.fact)
plotnScree(nS,main='碎石检测的非图形化解决ʽ',xlab = "因子", ylab = "value")
#最优因子3  加速因子1   简化  因子数量2

#  estimate
factor.result <- factanal(x=dat.fact, factor=2, scores="regression")
scree.plot(dat.fact)                                   # scree plot
names(factor.result)
print(factor.result)  #因子载荷矩阵A为10*2的    两个因子的累计方差贡献率是64.6%

#  plot loadings
load <- factor.result$loadings[,1:2] 
#提取因子分析结果的 因子载荷的两个公因子
#通过因子旋转，可使因子载荷矩阵
plot(load, type="n",xlab='因子1',ylab='因子2')                # set up plot 
#绘制因子载荷图，但并不显示图形
text(load, labels=names(dat.fact), cex=.7)                   # add variable names
#因子1： x6 5 4 9  利润因子
#因子2   x7 8   个股收益因子


##SVD
svd.result <- svd(dat.fact)
d <- svd.result$d
u <- svd.result$u
v <- svd.result$v
plot(svd.result$d, type = "b")

svd.matrix <- d[1]*as.matrix(u[,1])%*%t(as.matrix(v[,1])) + d[2]*as.matrix(u[,2])%*%t(as.matrix(v[,2])) + d[3]*as.matrix(u[,3])%*%t(as.matrix(v[,3]))

head(svd.matrix)
head(dat.fact)


##
ev <- eigen(cor(dat.fact)) # CPA计算相关系数矩阵的特征值  特征向量
svd.result <- svd(cor(dat.fact))  #SVD分解
ev$value
svd.result$d  #m=n 时SVD就是求特征值和特征向量

ev$vectors
svd.result$u
svd.result$v
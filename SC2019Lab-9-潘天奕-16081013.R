setwd('D:/R in finance/CH-02')
rm(list = ls())   
library(psy)

# 1. read data from EXCEL file
dat <- read.csv(file='factor.csv',encoding = "utf-8")
dat.fact <- dat[,-1]      # remove stock code
names(dat.fact) <- paste('x', 1:ncol(dat.fact), sep='')

# (2) estimate
factor.result <- factanal(x=dat.fact, factor=2, scores="regression")
psy::scree.plot(dat.fact)                                   # scree plot
names(factor.result)
print(factor.result)

# 3. plot loadings
load <- factor.result$loadings[,1:2] 
plot(load, type="n",xlab='因子1',ylab='因子2')                # set up plot 
text(load, labels=names(dat.fact), cex=.7)                   # add variable names



##########svd###############
svd.result <- svd(dat.fact)
d <- svd.result$d
u <- svd.result$u
v <- svd.result$v
plot(svd.result$d, type = "b")

svd.matrix <- d[1]*as.matrix(u[,1])%*%t(as.matrix(v[,1])) + d[2]*as.matrix(u[,2])%*%t(as.matrix(v[,2])) + d[3]*as.matrix(u[,3])%*%t(as.matrix(v[,3]))


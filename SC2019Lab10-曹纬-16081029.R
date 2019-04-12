library(readxl)                       # for xlsx data
library(nFactors)                     # for the optimal number of factors

dat <- read_excel("C:/Users/25070/Desktop/R语言/Code/CH-02/MicEcoData.xlsx", sheet='factor')
dat.fact <- dat[,-1]                  # remove stock code
names(dat.fact) <- paste('x', 1:ncol(dat.fact), sep='')

#PCA method
ev <- eigen(cor(dat.fact))                                   # calculate eigenvalues & eigenvalues of correlation matrix of dat.fact
psy::scree.plot(dat.fact)                                    # scree plot factor
factor.result <- factanal(x=dat.fact, factor=2, scores="regression");factor.result

load <- factor.result$loadings[,1:2]                         
plot(load, type="n",xlab='因子1',ylab='因子2')               # set up plot
text(load, labels=names(dat.fact), cex=.7)                   # add variable names

#SVD method
sc.dat <- scale(dat.fact)                                    # scale the matrix
s <- svd(sc.dat) 
s$d                                                          # the singular values of sc.dat
a <- t(sc.dat)
b <- as.matrix(sc.dat)
A <- a%*%b
sv <- s$d^2/63
nS <- nScree(sv)##确定最优因子数
plotnScree(nS,main='碎石测验的非图形化解决方式',xlab='因子',ylab='特征值')   

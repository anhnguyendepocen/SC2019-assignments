###SC2019Lab-9-Shuaidong Zhu-16081133
#example data
exdata <- matrix(c(71,74,92,79,
                   77,73,62,70,
                   80,84,95,88,
                   78,83,82,88,
                   75,82,96,77,
                   66,84,70,79,
                   70,83,66,70,
                   78,84,70,29,
                   72,87,78,49,
                   79,85,79,80),ncol = 4,byrow = T)
#SVD
svdresults <- svd(exdata)
svdresults
#Create a matrix with an initial value of 0
svd2matrix <- matrix(0, nrow = nrow(exdata), ncol = ncol(exdata)) 
ignorenum <- 2  #Set the number of minimum singular values to be removed
for (i in 1:(length(svdresults$d) - ignorenum))
{
  svd2matrix <- svd2matrix + svdresults$d[i] * as.matrix(svdresults$u[, i]) %*% t(as.matrix(svdresults$v[, i])) 
}
svd2matrix
#Measure the reduction degree of the whole information 
colME <- NULL    
for (j in 1:length(svdresults$d))
{
  colME <- c(colME, sum(abs(svd2matrix[, j] - exdata[, j])))
}

#Find the average absolute deviation
meanME <- sum(colME) / (nrow(exdata) * ncol(exdata))
meanME

#PCA
pcaresults <- prcomp(exdata,center = T)
#names(pcaresults)
#pcaresults
summary(pcaresults,loadings = T)

#Centralized processing
cendata <- scale(exdata,scale = F)
censvdresult <- svd(cendata)
#Comparing the results of PCA and SVD
#eigenvalue
(pcaresults$sdev)^2
censvdresult$d^2/(10-1)
#eigenvector
pcaresults$rotation
censvdresult$v

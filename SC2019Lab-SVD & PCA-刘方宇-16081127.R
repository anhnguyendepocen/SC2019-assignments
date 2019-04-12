data <- matrix(rnorm(80, 0, 1), 8, 10)
for(i in 1:10) data[,i] <- data[,i] - mean(data[,i])
#PCA
cov_data <- cov(data)
eigen_cov <- eigen(cov_data)
rank_eig <- order(eigen_cov$values, decreasing = T)
data_pca <- data %*% eigen_cov$vectors[,rank_eig[1:3]];data_pca
#SVD
svd.d = svd(data, nv = 10)
rank_d = order(svd.d$d, decreasing = T)
data_svd <- data %*% svd.d$v[,rank_d[1:3]];data_svd
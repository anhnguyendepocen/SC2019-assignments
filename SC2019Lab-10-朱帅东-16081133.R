###SC2019Lab-10-Shuaidong Zhu-16081133
setwd("G:/大三下学期/统计计算/Lab-10")
library(jpeg)
zsd <- readJPEG("zsd.jpg")
R <- zsd[,,1]
G <- zsd[,,2]
B <- zsd[,,3]
zsd_Rsvd <- svd(R)
zsd_Gsvd <- svd(G)
zsd_Bsvd <- svd(B)
rgb_svd_results <- list(zsd_Rsvd,zsd_Gsvd,zsd_Bsvd)

#Reassemble R,G,B matrices
reargb <- function(i){
  zsd.compress <- i$u[,1:j] %*% diag(i$d[1:j]) %*% t(i$v[,1:j])
  return(zsd.compress)
}
for (j in seq.int(50,800,length.out = 10)) {
  a <- sapply(rgb_svd_results, reargb ,simplify = "array")
  writeJPEG(a,paste('zsd_svd_rank_',round(j,0),'.jpg',seq = ''))
}
#Plot of singular values
x <- 1:length(zsd_Rsvd$d)
plot(c(x,x,x),c(zsd_Rsvd$d,zsd_Gsvd$d,zsd_Bsvd$d),type = "n",xlab = "index",ylab = "singular values",main ="Plot of singular values")
lines(x, zsd_Rsvd$d, lty=1, lwd=2,col = "red")
lines(x, zsd_Gsvd$d, lty=2, lwd=2, col = "green")
lines(x, zsd_Bsvd$d, lty=3, lwd=2,col = "blue")
legend('topright', legend=c('R', 'G', 'B'), lty=c(1,2,3), lwd=c(2,2,2),col = c("red","green","blue"))

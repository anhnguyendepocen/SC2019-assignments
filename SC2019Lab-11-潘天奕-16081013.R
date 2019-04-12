library(jpeg)
setwd("D:/statistical computing/photo")
Pan <- readJPEG("Pan Tianyi.jpg")
r <- Pan[,,1]
b <- Pan[,,2]
g <- Pan[,,3]
r.svd <- svd(r)
b.svd <- svd(b)
g.svd <- svd(g)
Pan.svd <- list(r.svd, b.svd,g.svd)
dim(r)
level.compress <- round(seq(4,413,length.out = 6))
for(i in level.compress){
  Pan.compress <- sapply(Pan.svd,function(tmp){
    tmp$u[,1:i]%*%diag(tmp$d[1:i])%*%t(tmp$v[,1:i])
  },simplify = "array")
  filename <- paste("Pan.compress-","num.d-",i,".jpg",seq = "")
  #writeJPEG(Pan.compress,filename)
  
}
plot(r.svd$d,type = "l", ylab = "singular value")



library(jpeg)
pic <- readJPEG('E:/picture/dragonball.jpg')
R <- pic[,,1]
G <- pic[,,2]
B <- pic[,,3]
pic.R.svd <- svd(R)
pic.G.svd <- svd(G)
pic.B.svd <- svd(B)
plot(pic.R.svd$d, type = 'l')
plot(pic.G.svd$d, type = 'l')
plot(pic.B.svd$d, type = 'l')
rgb.svds <- list(pic.R.svd, pic.G.svd, pic.B.svd)
for(i in seq.int(4,1004,100)){
  new_pic <- sapply(rgb.svds, function(x){
    pic.compress <- x$u[, 1:i] %*% diag(x$d[1:i]) %*% t(x$v[,1:i])
  }, simplify = 'array')
  writeJPEG(new_pic, paste('E:/picture/dragonball_svd_rank_', i, 
                           '.jpg', sep=''))
}
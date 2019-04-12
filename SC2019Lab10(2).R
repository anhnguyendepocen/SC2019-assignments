library(jpeg)
tatan <- readJPEG("C:/Users/25070/Desktop/tatan.jpg")
r <- tatan[,,1]
g <- tatan[,,2]
b <- tatan[,,3]
tatan.rsvd <- svd(r)
tatan.gsvd <- svd(g)
tatan.bsvd <- svd(b)
rgb.svd <- list(tatan.rsvd,tatan.gsvd,tatan.bsvd)
for (i in seq.int(4,200,length.out = 8)) {
  a <- sapply(rgb.svd, function(j){
    t.compress <- j$u[,1:i] %*% diag(j$d[1:i]) %*% t(j$v[,1:i])
  },simplify = 'array')
  writeJPEG(a,paste('C:/Users/25070/Desktop/','tatansvd',round(i,0),'.jpg',sep = ''))
}

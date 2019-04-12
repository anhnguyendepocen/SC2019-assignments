## take a high resolution image of yourself, and produce a sequence of low rank approximations.
## 载入图片，并且显示出来
library(jpeg)
photo <- readJPEG("C:/Users/THINK/Desktop/PHOTO.jpg")

## 建立三色通道
r <- photo[, , 1]
g <- photo[, , 2]
b <- photo[, , 3]
svdr <- svd(r)
svdg <- svd(g)
svdb <- svd(b)
svdrgb <- list(svdr, svdg, svdb)

## 对比显示
for (j in seq.int(50, 1000, length.out = 6)) {
  ans <- sapply(svdrgb, function(i){
    compress <- i$u[, 1:j] %*% diag(i$d[1:j]) %*% t(svdr$v[, 1:j])
  }, simplify = 'array')
  writeJPEG(ans, paste('C:/Users/THINK/Desktop/', round(j,0), '.jpg'))
}





############################################################
## Peek into SVD and PCA in R, illustrate their relationship

## Part 1
## apply svd  method to image processing
## compare images consisting of different amount of information,from the least to the most
## display image information carried by different singular values

## Part 2
## PCA in image processing
## reduce the number of matrices of the raw data
## reduce the dimension(or number of columns) of every matrix (red,green,blue)of the raw data
############################################################


setwd("E:/好好学习 天天向上/各科复习资料/统计计算")

## get the raw data
library(jpeg)
photo <- readJPEG("ride.jpg")
red <- photo[,,1]
green <- photo[,,2]
blue <- photo[,,3]

##what's the picture drawn from some of the raw data?
writeJPEG(red,"ride_red.jpg")
writeJPEG(blue,"ride_blue.jpg")
writeJPEG(green,"ride_green.jpg")

## Now apply svd  method to image processing
r.svd <- svd(red)
g.svd <- svd(green)
b.svd <- svd(blue)
rgb.svds <- list(r.svd,g.svd,b.svd)

## draw four image consisting of different amount of information
## ,from the least to the most,then compare 
for (j in seq(10,300,length.out = 4)) {
  a <- sapply(rgb.svds, function(i){
    photo.compress <- i$u[,1:j]%*%diag((i$d[1:j]))%*%t(i$v[,1:j])
    },simplify = 'array')
  writeJPEG(a,paste("ride_svd",round(j,0),".jpg",sep = ""))
}

## Display image information carried by different singular values
## Here only compare the top six singular values
for (j in 1:6) {
  a <- sapply(rgb.svds, function(i){
    photo.compress <- i$d[j]*(i$u[,j])%*%t(i$v[,j])
  },simplify = 'array')
  writeJPEG(a,paste("ride_svd00",j,".jpg",sep = ""))
}


##########################
###PCA in image processing

library(nFactors)

## reduce the number of matrices of the raw data
## (convert the 238*358*3 array to 238*258*i array)

## firstly,convert the 238*358*3 array to 85204*3 matrix
r1 <- matrix(red,85204,1)
g1 <- matrix(green,85204,1)
b1 <- matrix(blue,85204,1)
rgb1 <- cbind(r1,g1,b1)

for (i in 1:3) {
  p <- pca(rgb1,nfactors = i,n.obs = 85204)$scores
  p <- array(p,c(238,358,i))
  writeJPEG(p,paste("ride_pca",i,".jpg",sep = ""))
}


## ues PCA to reduce the dimension(or number of columns) of every matrix (red,green,blue)of the raw data
rgb <- list(red,green,blue)
for (j in seq(3,478,length.out = 4)) {
  p <- sapply(rbg, function(i){
  a <- pca(i,nfactors = j,n.obs = 318)$scores
  }
  ,simplify = 'array')
  writeJPEG(p,paste("ride_pca_compress",round(j,0),".jpg",sep = ""))
}


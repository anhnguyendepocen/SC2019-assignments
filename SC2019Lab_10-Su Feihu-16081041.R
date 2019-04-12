###SC2019Lab_10-Su Feihu-16081041
###NAME:Su Feihu  STUDENTID:16081041


##Reconstructing approximated image
setwd("D:/study/statistical computing/Lab")

#function to reconstruct image 
image.reconstruct <- function(image, rank)
{
  #image is the array of the image
  #rank is the Pixel you want to get
  
  #get the matrix of three color
  image.r <- image[ , , 1]
  image.g <- image[ , , 2]
  image.b <- image[ , , 3]
  
  #if the rank is large than m/n.
  if(rank > min(dim(image.ori)[1:2])) {stop("the rank is too large.")}
  
  #svd
  image.r.svd <- svd(image.r)
  image.g.svd <- svd(image.g)
  image.b.svd <- svd(image.b)
  rgb.svds <- list(image.r.svd, image.g.svd, image.b.svd)
  
  #compress the matrix
  compress <- function(svd)
  {
    image.compress <- svd$u[,1: rank] %*% diag(svd$d[1: rank]) %*% t(svd$v[,1: rank])
  }
  
  #get the array of approximated image
  reconstruct <- sapply(rgb.svds, compress, simplify = "array")
  
  return(reconstruct)
}

#import the image
library(jpeg)
image.ori <- readJPEG("./honghong.jpg")
dim(image.ori)

#write the image
for (rank in c(600, 495, 286, 200, 116, 32)){
  writeJPEG(image.reconstruct(image.ori, rank), paste("./honghong_svd_rank_", rank, ".jpg", sep =""))
}

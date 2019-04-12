#lab11 take a high resolution image of yourself, and produce a sequence of low rank approximations.
#first read the image in R
library(jpeg)
img <- readJPEG("qhphoto.jpeg")
# writeJPEG(img,'photos.jpg',quality = 0.3)
str(img)
head(img)

#we can find that the img is a array consists of three 622*400 matrices
#use svd for each matrix
R <- img[,,1]
r.svd <- svd(R)

#curve the tendency of the singular value
plot(1:length(r.svd$d), r.svd$d, pch = 20, main = "plot of the singular value", 
     xlab = "index", ylab = "singular value")
lines(1:length(r.svd$d),r.svd$d)

k = c(300,275,250,200,177,100,50,10)
filename = paste('photo', 1:length(k), sep = "")

for (j in 1:length(k)) {
  dimnum = k[j]
  compress.image <- array(0,dim = c(413,307,3))
  
  for (i in 1:3) {
    image.matrix <- img[,,i]
    image.svd <- svd(image.matrix)
    
    u <- image.svd$u
    v <- t(image.svd$v)
    d <- diag(image.svd$d)
    
    u1 = u[,1:dimnum]
    v1 = v[1:dimnum,]
    d1 = d[1:dimnum,1:dimnum]
    temp = u1%*%d1%*%v1
    
    compress.image[,,i] = temp
  }
  writeJPEG(image = compress.image, target = paste0(filename[j],'.jpg'))
  
}
#the original image size is 11390b,
#while the compressed image size is 10837b,10841b,10883b,11241b,18kb,19kb,15kb
#当选取的奇异值数量相对小时，能够起到压缩图片大小且保证了图片的清晰度
#但是当选取奇异值数量很小时，图片会出现失真且图片大小反而会变大，
#我个人推断可能是计算后矩阵中小数位数过多，导致存储量变大或者重新合成图片过程中有什么原因

#下面是我写的压缩图片的函数，filename-文件名,type-文件类型,kpercent-选择的奇异值的比例
image_compress <- function(filename, kpercent = 0.5, type,newname = 'compression.'){
  library(jpeg)
  image = readJPEG(filename)
  
  r = min(dim(image)[1], dim(image)[2])#rank for the matrix
  k = kpercent*r#selected singular values numbers
  
  if (is.na(dim(image)[3])) {
    image.svd = svd(image)
    u = image.svd$u
    v = image.svd$v
    sv = diag(image.svd$d)
    if (nrow(u) > r) {
      s = u[,1:k] %*% sv[1:k,1:k] %*% t(v)[1:k,]
    }else{
      s = u[,1:k] %*% sv[1:k,1:k] %*% v[1:k, ]
    }
    writeJPEG(s, paste0(newname,type))
    
  }else{
    compress.image <- array(0,dim = dim(image))
    
    for (i in 1:3) {
      image.matrix <- image[,,i]
      image.svd <- svd(image.matrix)
      
      u <- image.svd$u
      v <- image.svd$v
      d <- diag(image.svd$d)
      
      if(nrow(u)>r){
        temp = u[,1:k] %*% d[1:k,1:k] %*% t(v)[1:k,]
      }else{
        temp = u[,1:k] %*% d[1:k,1:k] %*% v[1:k,]
      }
      compress.image[,,i] = temp
    }
    writeJPEG(compress.image, paste0(newname,type))
    
  }
}
#我对蒙娜丽莎的图片进行压缩，并没有出现刚才的问题，图片会随着比例的减小而减小
image_compress('monna.jpeg', kpercent = 0.75, type = 'jpeg')
image_compress('monna.jpeg', kpercent = 0.5, type = 'jpeg')
image_compress('monna.jpeg', kpercent = 0.25, type = 'jpeg')
image_compress('monna.jpeg', kpercent = 0.1, type = 'jpeg')
image_compress('monna.jpeg', kpercent = 0.05, type = 'jpeg')
image_compress('monna.jpeg', kpercent = 0.01, type = 'jpeg')


#Use generalised inverse to solve a linear regression problem
#dat is a dataframe with 8 variables and 41 observations
dat <- read_xlsx(path = 'MicEcoData.xlsx', sheet = 'stepReg')
names(dat) <- c('y', paste('x', 1:7, sep=''))
head(dat)

x.matrix = c(rep(1, length(dat$x1)), dat$x1, dat$x2, dat$x3, dat$x4, dat$x5, dat$x6, dat$x7)
X = matrix(x.matrix, ncol = 8)
Y = matrix(dat$y, ncol = 1)
#use generalised inverse to calculate the coefficients beta
(beta = ginv(X) %*% Y)

#compare the result with lm()
lmod <- lm(y~., data = dat)
summary(lmod)
lmod$coefficients
#we could find that the result is equivalent to lm() 

#use the step model to compare generalised inverse with lm()
mod.step <- step(lmod)
summary(mod.step)
mod.step$coefficients

x = matrix(c(rep(1, length(dat$x1)), dat$x1, dat$x4, dat$x5, dat$x6, dat$x7), ncol = 6)
beta1 = ginv(x) %*% Y
beta1
#beta1 is the same as the lm() coefficients
#in the help of the lm(),the method for fitting is qr decomposition，
#from which we get the same result with generalised inverse


####SC2019Lab-9-ÕÔÈüÎ°-16081108####
##getting data##
data(USArrests, package = "datasets") 
#data on four crime indicators in 50 U.S. States

data <- USArrests  #for easy to write
summary(USArrests)

####for PCA
library(stats) #library package
summary(pca.data <- princomp(data, cor = T))
#the Cumulative Proportion of comp.1 and comp.2 is 86.75%
loadings(pca.data)
##getting:
##Comp1 = (-0.536) * Murder + (-0.583) * Assault + (-0.278)*UrbanPop + (-0.543)* Rape;
##Comp2 = 0.418 * Murder + 0.188 * Assault + (-0.873)*UrbanPop + (-0.167)* Rape
data.pca <- data.frame(pca.data$scores[,-c(3,4)])
#get scores of comp.1 and comp.2

library(ggplot2)
ggplot(data.pca, aes(x = Comp.1, y = Comp.2)) + xlab("First Component") + 
  ylab("Second Component") + geom_text(alpha = 0.75, label = rownames(data.pca), size = 4)
##draw component diagram

####for SVD
data.matrix <- t(as.matrix(data)) #turn it to matrix
dim(data.matrix) #dimensions of matrix
svd.data <- svd(data.matrix) #svd

d <- diag(svd.data$d)  #get every matrix
v <- as.matrix(svd.data$v)
u <- svd.data$u
d1 <- as.matrix(d[c(1,2),c(1,2)]) #turn to two dimensions
v1 <- as.matrix(v[,c(1,2)])
u1 <- as.matrix(u[c(1,2),c(1,2)])

data.svd <- u1 %*% d1 %*% t(v1) #get new data


####standardization
data_standard <- scale(data,center=T,scale=T)  #standardization of date

####for PCA of standard data
summary(pca.data_standard <- princomp(data_standard, cor = T))
#the Cumulative Proportion of comp.1 and comp.2 is 86.75%
loadings(pca.data_standard)
##getting:
##Comp1 = (-0.536) * Murder + (-0.583) * Assault + (-0.278)*UrbanPop + (-0.543)* Rape;
##Comp2 = 0.418 * Murder + 0.188 * Assault + (-0.873)*UrbanPop + (-0.167)* Rape
data.pca_standard <- data.frame(pca.data_standard$scores[,-c(3,4)])
#get scores of comp.1 and comp.2

library(ggplot2)
ggplot(data.pca, aes(x = Comp.1, y = Comp.2)) + xlab("First Component") + 
  ylab("Second Component") + geom_text(alpha = 0.75, label = rownames(data.pca), size = 4)
##draw component diagram

####for SVD of standard data
data.matrix_standard <- t(as.matrix(data_standard))
dim(data.matrix_standard) #dimensions of matrix
svd.data_standard <- svd(data.matrix_standard)

d_standard <- diag(svd.data_standard$d)
v_standard <- as.matrix(svd.data_standard$v)
u_standard <- svd.data_standard$u
d1_standard <- as.matrix(d_standard[c(1,2),c(1,2)])
v1_standard <- as.matrix(v_standard[,c(1,2)])
u1_standard <- as.matrix(u_standard[c(1,2),c(1,2)])

data.svd_standard <- u1_standard %*% d1_standard %*% t(v1_standard)

#####Data compression of images
library(raster)
library(jpeg)

##get photo
lion.photo <- raster("C:/Users/Lenovo/Desktop/lion.jpg")

##turn to matrix
photo.matrix <- t(as.matrix(lion.photo))
dim(photo.matrix)
image(photo.matrix, col = grey(seq(0, 1, length = 256))) ##grey it

##svd of image
photo.svd <- svd(photo.matrix)
photo.d <- diag(photo.svd$d)
photo.v <- as.matrix(photo.svd$v)
photo.u <- photo.svd$u

photo.u1 <- as.matrix(photo.u[,1:100])
photo.d1 <- as.matrix(photo.d[1:100,1:100])
photo.v1 <- as.matrix(photo.v[,1:100])
photo1 <- photo.u1 %*% photo.d1 %*% t(photo.v1)

photo.u2 <- as.matrix(photo.u[,1:50])
photo.d2 <- as.matrix(photo.d[1:50,1:50])
photo.v2 <- as.matrix(photo.v[,1:50])
photo2 <- photo.u2 %*% photo.d2 %*% t(photo.v2)

photo.u3 <- as.matrix(photo.u[,1:30])
photo.d3 <- as.matrix(photo.d[1:30,1:30])
photo.v3 <- as.matrix(photo.v[,1:30])
photo3 <- photo.u3 %*% photo.d3 %*% t(photo.v3)

photo.u4 <- as.matrix(photo.u[,1:20])
photo.d4 <- as.matrix(photo.d[1:20,1:20])
photo.v4 <- as.matrix(photo.v[,1:20])
photo4 <- photo.u4 %*% photo.d4 %*% t(photo.v4)

photo.u5 <- as.matrix(photo.u[,1:10])
photo.d5 <- as.matrix(photo.d[1:10,1:10])
photo.v5 <- as.matrix(photo.v[,1:10])
photo5 <- photo.u5 %*% photo.d5 %*% t(photo.v5)

photo.u6 <- as.matrix(photo.u[,1])
photo.d6 <- as.matrix(photo.d[1,1])
photo.v6 <- as.matrix(photo.v[,1])
photo6 <- photo.u6 %*% photo.d6 %*% t(photo.v6)

##the change of image
image(photo1, col = grey(seq(0, 1, length = 256)))
image(photo2, col = grey(seq(0, 1, length = 256)))
image(photo3, col = grey(seq(0, 1, length = 256)))
image(photo4, col = grey(seq(0, 1, length = 256)))
image(photo5, col = grey(seq(0, 1, length = 256)))
image(photo6, col = grey(seq(0, 1, length = 256)))

#####generalised inverse to solve lm#####
##get data
data(wcgs, package = "faraway")
x1 <- wcgs[,3]
x2 <- wcgs[,8]
chd <- wcgs[,10]

##get x1,x2,y
y <- c()   
for(i in 1:3154){
  if(chd[i] == "no"){y[i] = 0}
  else{y[i] = 1}
}
y <- as.matrix(y)
dim(y)

x <- cbind(x1,x2)
dim(x)

library(MASS)
beta <- ginv(x) %*% y  #get beta
beta
eta <- t(y - x %*% beta) %*% (y - x %*% beta) #get Residual value
eta

lm <- lm(y~x1+x2)
summary(lm)

##for function
lm_test <- function(x,y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(dim(x)[1] == dim(y)[1]){
    beta <- ginv(x) %*% y  #get beta
    eta <- t(y - x %*% beta) %*% (y - x %*% beta) #get Residual value
    print(beta)
    print(paste("eta is",eta))
  }
  else{
    print("someting wrong with the data")
  }
}

x <- matrix(c(23,53,13,74,437,52,8,57,22),3,3)
y <- c(5,2,3)
lm_test(x,y)

summary(lm(y~x[,1]+x[,2]+x[,3]))

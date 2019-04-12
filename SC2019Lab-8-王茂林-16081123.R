################################
setwd('C:\\Users\\hp\\Desktop\\BTD\\Õ≥º∆º∆À„')
library(jpeg)
library(ggplot2)
library(reshape2)
pic<-readJPEG('charlotte.jpg')
r<-pic[,,1]
g<-pic[,,2]
b<-pic[,,3]
r_svd<-svd(r)
g_svd<-svd(g)
b_svd<-svd(b)
rgb_svd<-list(r_svd,g_svd,b_svd)
high<-length(b_svd$d)#It require about 5% that the picture is clear
for(j in seq.int(4,0.05*high,length.out = 10)){
  s<-sapply(rgb_svd,function(i){com<-i$u[,1:j]%*%diag(i$d[1:j])%*%t(i$v[,1:j])},simplify = 'array')
  writeJPEG(s,paste('charlotte',round(j,0),'.jpg',seq='' ))
}


pic<-readJPEG('zero.jpg')
r<-pic[,,1]
g<-pic[,,2]
b<-pic[,,3]
r_svd<-svd(r)
g_svd<-svd(g)
b_svd<-svd(b)
rgb_svd<-list(r_svd,g_svd,b_svd)
high<-length(b_svd$d)
for(j in seq.int(4,0.05*high,length.out = 10)){
  s<-sapply(rgb_svd,function(i){com<-i$u[,1:j]%*%diag(i$d[1:j])%*%t(i$v[,1:j])},simplify = 'array')
  writeJPEG(s,paste('zero',round(j,0),'.jpg',seq='' ))
  }

library(jpeg) #load package
y<-readJPEG('C://LJJ.jpg') #import image
r<-y[,,1]
g<-y[,,2]
b<-y[,,3]
y.r.svd<-svd(r)
y.g.svd<-svd(g)
y.b.svd<-svd(b)
rgb.svds<-list(y.r.svd,y.g.svd,y.b.svd)
for(j in seq.int(4,200,length.out = 8)){ #set the value of k
  a<-sapply(rgb.svds,function(i){ 
    y.compress<-i$u[,1:j]%*%diag(i$d[1:j])%*%t(i$v[,1:j])
  },simplify = 'array')
  writeJPEG(a,paste('C://temp/','LJJ_svd_rank_',round(j,0),'.jpg',sep='')) #output compressed image
}

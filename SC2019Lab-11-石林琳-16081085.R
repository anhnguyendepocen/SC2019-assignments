library(jpeg)
lin<-readJPEG("C:/Users/Por/Desktop/R/lin.jpg")
r<-lin[,,1]
g<-lin[,,2]
b<-lin[,,3]
r.svd<-svd(r)
g.svd<-svd(g)
b.svd<-svd(b)
rgb.svds<-list(r.svd,g.svd,b.svd)
for (j in seq.int(100,400,length.out = 4))
{
  a<-sapply(rgb.svds,function(i){
    lin.compress<-i$u[,1:j]%*%diag(i$d[1:j])%*%t(i$v[,1:j])
  },simplify = 'array')
  writeJPEG(a,paste('C:/Users/Por/Desktop/R/','lin_svd_rank',round(j,0),'.jpg',sep=''))
}

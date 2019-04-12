##SC-lab-10
#图片RGB矩阵svd分解及重组
getwd()
setwd("C:/Users/Administrator/Desktop/Rdata/SC-L10")
library(jpeg)
list.files()

graph1<-readJPEG("test1.jpg")
summary
dim(r)
head(graph1)
r<-graph1[,,1]
g<-graph1[,,2]
b<-graph1[,,3]

rsvd<-svd(r)
gsvd<-svd(g)
bsvd<-svd(b)

rgb.svd<-list(rsvd,gsvd,bsvd)

for (i in seq.int(2,200,length.out = 8)) {
  a<-sapply(rgb.svd, function(x){
    graph.compress<-x$u[,1:i] %*% diag(x$d[1:i]) %*% t(x$v[,1:i])
  },
  simplify = 'array'
  )
  writeJPEG(a,paste('output_',round(i,0),'.jpg',sep = ''))
}
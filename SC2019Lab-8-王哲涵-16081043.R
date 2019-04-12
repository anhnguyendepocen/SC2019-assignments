A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)#对A进行赋值

a<-colSums(A)
a[a==0]<-1
n <- nrow(A)
T<-matrix(0,nrow(A),ncol(A))#创造一个与A大小相同的0矩阵T
for (i in 1:n) T[i,]<-T[i,]+A[i,]/a
n<-nrow(T)
x<-rep(1,n)
for (i in 1:1000) x<-T%*%x
r<-x/sum(x)#对x中的元素进行加权计算
rank(r)
r
sum(r)
# QR  
b<-matrix(c(1,2,3,4),2,2)
i=1
Q=matrix(c(1,0,1,0),2,2)
while(i<100){
  x<-b
  a<-qr(x,tol = 1e-07)
  q<-qr.Q(a)
  r<-qr.R(a)
  b<-r%*%q
  Q<-Q%*%q
  i<-i+1
  QR<-q%*%r
}
QR
Q


#The Power Method
A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)#对A进行赋值
a<-colSums(A)
a[a==0]<-1 
n<-nrow(A)
T<-matrix(0,nrow(A),ncol(A))
for (i in 1:n) T[i,]<-T[i,]+A[i,]/a
n<-nrow(T)
x<-rep(1,n)
for (i in 1:100) x<-T%*%x
r<-x/sum(x)
rank(r)

#svd&lm

##lm
plot(women$height, women$weight,
     xlab = "Height", ylab = "Weight")
fit <- lm(weight ~ height, data = women)
summary(fit)

##svd
T <- svd(women$height)
U <- T$u
D <- T$d
V <- T$v
A<- V%*%solve(D)%*%t(U)
B<-A%*%women$weight

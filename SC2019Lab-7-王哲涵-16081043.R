f<-function(X){
  x1<-X[1]
  x2<-X[2]
  x1^2+x2^2
}
A=c(1,1)
B=c(1,2)
C=c(2,2)
x<-rbind(A,B,C)
y<-rbind(f(A),f(B),f(C))
r2=1
r3=2
while(sqrt((x[r2,1]-x[r3,1])^2+(x[r2,2]-x[r3,2])^2)>0.0001){
  h<-rank(y)
  r1<-which(h==3)
  r2<-which(h==2)
  r3<-which(h==1)
  R<-c(as.numeric(x[r2,1]+x[r3,1]-x[r1,1]),as.numeric(x[r2,2]+x[r3,2]-x[r1,2]))
  
  if (y[r3]<f(R)<y[r1]){
    y[r1]<-f(R)
    x[r1,1]<-R[1]
    x[r1,2]<-R[2]
  } 
  else if (y[r3]-f(R)>0){
    Q<-c(as.numeric(x[r2,1]*1.5+x[r3,1]*1.5-x[r1,1]*2),as.numeric(x[r2,2]*1.5+x[r3,2]*1.5-x[r1,2]*2))
    if (f(Q)<f(R)){
      y[r1]<-f(Q)
      x[r1,1]<-Q[1]
      x[r1,2]<-Q[2]
    }
    else{
      y[r1]<-f(R)
      x[r1,1]<-R[1]
      x[r1,2]<-R[2]
    }
  }
  else{
    R<-c(as.numeric(x[r2,1]*0.25+x[r3,1]*0.25+x[r1,1]*0.5),as.numeric(x[r2,2]*0.25+x[r3,2]*0.25+x[r1,2]*0.5))
    y[r1]<-f(R)
    x[r1,1]<-R[1]
    x[r1,2]<-R[2]
  }
}
print(y[1])
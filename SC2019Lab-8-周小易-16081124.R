#lab—8


#单纯形优化
DFO_method<-function(FUN,x0,n=1000,volume=10^(-5)){
  library(animation)
  ani.record(reset = TRUE)
  x<-x0
  l<-length(x0)
  num<-0
  f<-sapply(x, FUN)
  while(num<n){
    
    xplot<-t(data.frame(x,x[1]))
    xplot[4]<-xplot[1]
    
    filled.contour(x = -5:5, y = -5:5, z = z,nlevels = 10,
                   color.palette =heat.colors,
                   plot.axes = { axis(1); axis(2);
                     lines(xplot[,1],xplot[,2]);
                     points(xplot,pch=20)
                   }) 
    
    ani.record()
    
    #判断最大值点
    k<-which.max(f)
    x_n<-x[[k]]
    sumx<-c(0,0)
    
    for (j in 1:l) {
      if(j!=k)  sumx<-sumx+x[[j]]
    }
    x_<-sumx/l  #质点
    
    xr<-x_+x_-x[[k]]
    #最小值点
    kmin<-which.min(f)
    x1<-x[[kmin]]

    #三种情况
    if(FUN(xr)>max(f[-k])){
      #若反射点还是最大值点，寻找一个收缩点xc
      xc<-x_+0.5*(x_n-x_)
      
      if(FUN(xc)>=FUN(x_n)){
        #若收缩点依然为最大值点，缩小单纯型
        for(i in 1:l)  x[[i]]<-0.5*(x[[i]]+x1)
      }
      else {
        #否则取xc
        x[[k]]<-xc
        f[k]<-FUN(xc)
      }
    }
    else if(FUN(xr)<FUN(x1)){
      #若反射点成为最小值点，说明方向较优，沿方向继续下降
      xe<-x_+2*(xr-x_)
      if(FUN(xr)>FUN(xe)){
        x[[k]]<-xe
        f[k]<-FUN(xe)
      }
      else {
        x[[k]]<-xr
        f[k]<-FUN(xr)
      }
    }
    else {
      #若反射点在最大最小值之间，进行下一轮循环
      x[[k]]<-xr
      f[k]<-FUN(xr)
    }
    
    #停止条件
    num<-num+1
    xm<-list()
    for(i in 1:(l-1))  xm[[i]]<-x[[i]]-x[[l]]
    
    xv<-sapply(xm, cbind)
    v<-1/2*abs(det(xv))
    if(v<volume) break
  }
  oopts = ani.options(interval = 0.5)
  return(list(f=f,x=x,num=num))
}

ftest1<-function(x) x[1]^2+x[2]^2

ftest2<-function(x) x[1]^2+x[2]^2-5

x0<-list(c(1,1),c(1,2),c(2,2))

xy <- expand.grid(-5:5, -5:5)
z <- matrix(xy[,1]^2 + xy[,2]^2, 11)
                   
DFO_method(ftest1,x0=x0,1000)

ani.replay()
saveGIF(ani.replay(), movie.name = "gif_1.gif")

#特征根

A<-matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5)
A
?eigen
x<-eigen(A)
x
r<-x$vectors[,1]
a<-x$values[1]
abs(r)
A %*% r /r
rank(r)

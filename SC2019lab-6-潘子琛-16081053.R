#样本
x<-seq(1,10,0.25)
y<-c(0,1,1,0,1,1,0,1,1,1,0,0,1,0,0,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0) 
plot(x,y)

#定义logitlik函数
logitlik<-function(theta){
  beta0<-theta[1]
  beta1<-theta[2]
  p=1/(1+exp(-beta0-beta1*x))
  f<-sum(y*log(p)+(1-y)*log(1-p))  #似然函数
  j<-c(sum(y*(exp(-beta0-beta1*x)*p)+(y-1)*p),sum(y*(x*exp(-beta0-beta1*x)*p)+(y-1)*x*p))  #偏导数
  #hessi矩阵
  h<-matrix(c(sum((-y)*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
            sum(-y*x*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
            sum(-y*x*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
            sum(-y*x^2*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x^2*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2))),nrow=2,ncol=2,byrow = T)
                
  list(f=f,j=j,h=h)
}

Newtons<-function(fun,x,ep=1e-15,it_max=1000) {
  index<-0;k<-0   #初始赋值
  repeat{
    k<-k+1;
    x1<-x;obj<-fun(x);
    x<-x-solve(obj$h,obj$j)
    norm<-sqrt((x-x1)%*%(x-x1))
    if(norm<ep){   #终止条件
      index<-1;
      break;      #终止循环
    } 
  }
  obj<-fun(x);
  list(root=x,it=k,index=index,Fval=obj$f);  
}
#初始值不相同的两次迭代
debug(Newtons)
optimOut1 <- Newtons(logitlik,c(0.1,0.2))
optimOut1
optimOut2 <- Newtons(logitlik,c(0,0))
optimOut2





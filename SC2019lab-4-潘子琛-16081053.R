Newtons<-function(fun,x,ep=1e-15,it_max=1000){#fun是函数，x表示初值，ep表示精度，itmax表示最大迭代次数
  index<-0;k<-1
  while(k<=it_max){
    x1<-x;obj<-fun(x);
    x<-x-solve(obj$j,obj$f)
    norm<-sqrt((x-x1)%*%(x-x1))  
    if(norm<ep){
      index<-1;break
    }
    k=k+1
  }
  obj<-fun(x)
  list(root=x,it=k,index=index,Fval=obj$f)
}


fun1<-function(x){
  f<-x^2-5
  j<-2*x
  list(f=f,j=j)
}

Newtons(fun1,3)

fun2<-function(x){
  f<-x^2^(1/4)
  j<-0.5*x*(x^2)^(-0.75)
  list(f=f,j=j)
}

Newtons(fun2,0.25)

fun3<-function(x){
  f<- x*exp(-x^2)-0.4*(1+exp(x))^(-1)-0.2
  j<- (1-2*x^2)*exp(-1*x^2)+0.8*exp(x)*(1+exp(x))^(-2)
  list(f=f,j=j)
}

Newtons(fun3,0.5)
Newtons(fun3,0.6)


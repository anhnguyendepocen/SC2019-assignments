#二元函数的表达式，将其一、二阶导函数写出并记录
fun<-function(x){
  f<-x[1]^2-x[1]*x[2]+x[2]^2+exp(x[2])     #原函数
  j<-c(2*x[1]-x[2],-x[1]+2*x[2]+exp(x[2]))  #偏导数
  h<-matrix(c(2,-1,-1,2+exp(x[2])), nrow=2,ncol=2,byrow = T) #海塞矩阵
  list(f=f,j=j,h=h)
}

#fun是函数，x表示初值，ep表示精度，itmax表示最大迭代次数
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

debug(Newtons)
Newtons(fun,c(1,2))



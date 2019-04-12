newton.raphson<-function(fun,x0,tol=0.000001,max=100){
  x<-x0
  for(i in 1:max){
    obj<-fun(x)
    x<-x-obj$f/obj$df
    if(abs(x-x0)<tol){
      return(x)
      break
    }
    x0<-x
    i=i+1
  }
}

fun1<-function(x){
  f<-x^2-5
  df<-2*x
  list(f=f,df=df)
}
newton.raphson(fun1,2)

fun2<-function(x){
  f<-(x^2)^0.25
  df<-0.5*x^(-0.5)
  list(f=f,df=df)
}
newton.raphson(fun2,0.25) #has something wrong QAQ

fun3<-function(x){
  f<-x*exp(-x^2)-0.4/(exp(x)+1)-0.2
  df<-(1-2*x^2)*exp(-x^2)+0.4*exp(x)/(exp(x)+1)^2
  list(f=f,df=df)
}
newton.raphson(fun3,0.5)
newton.raphson(fun3,0.6)

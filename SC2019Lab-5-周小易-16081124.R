
#lab-5
#Newton-Raphson Method
#The output of “FUN” is a vector
#including the values of the function and derivative
#x0 : initial value
#er : the tolarance of error
#n  : The maximum number of iterations
newton.raphson<-function(FUN,x0,er,n){
  
  x<-x0
  fun<-FUN(x)
  k<-0
  
  while (abs(fun[1])>er & k<n) {
    x<-x-fun[1]/fun[2]
    fun<-FUN(x)
    k<-k+1
  }
  
  if(abs(fun[1])<er) cat("误差已小于",er,sep="")
  if(k==n) cat("已经经过", n ,"次迭代,未能收敛",sep="")
    
  return(list(x=x,k=k,fx=fun[1]))
}


#1. find the root of x2 = 5.
fx1<-function(x){
  y<-x^2-5
  dy<-2*x
  return(c(y,dy))
}

x1<-  newton.raphson(fx1,x0=1,er=1*10^(-5),n=10)
x1


#2. find the root of f(x) = √ | x | . Try initial value 0.25.
fx2<-function(x){
  y<-(abs(x))^0.5
  if (x>0) {
    dy<-(1/2)*x^(-1/2)
  }
  else {
    dy<-(-1/2)*(-x)^(-1/2)
  }
  return(c(y,dy))
}

x2<-newton.raphson(fx2,x0=0.25,er=1*10^(-5),n=10)
x2

#newton法对于某些特殊情况无法收敛

#3.find the root of xe − x2 = 0.4(ex + 1) − 1 + 0.2. 
#  Try initial values 0.5 and 0.6.
fx3<-function(x){
  y<-x*exp(-x^2)-0.4*(exp(x)+1)^(- 1)-0.2
  dy<-exp(-x^2)+x*(-2*x)*exp(-x^2)+0.4*exp(x)*(exp(x)+1)^(- 2)
  return(c(y,dy))
}

x3<-newton.raphson(fx3,x0=0.5,er=1*10^(-5),n=10)
x3
x4<-newton.raphson(fx3,x0=0.6,er=1*10^(-5),n=10)
x4

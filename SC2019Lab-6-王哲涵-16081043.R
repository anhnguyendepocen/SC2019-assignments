x<-seq(1,20,0.5)
y<-c(0,1,1,0,1,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,1,0,0,0,0,1,1,0,1,0,1,0,1,0,0,1,0) 

logistic<-function(a){
  beta0<-a[1]
  beta1<-a[2]
  p=1/(1+exp(-beta0-beta1*x))
  lnL<-sum(y*log(p)+(1-y)*log(1-p))
  grad<-c(sum(y*(exp(-beta0-beta1*x)*p)+(y-1)*p),sum(y*(x*exp(-beta0-beta1*x)*p)+(y-1)*x*p))
  hess<-matrix(c(sum(-y*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
                 sum(-y*x*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
                 sum(-y*x*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2)),
                 sum(-y*x^2*exp(beta0+beta1*x)*(1+exp(beta0+beta1*x))^(-2)+(y-1)*x^2*exp(-beta0-beta1*x)*(1+exp(-beta0-beta1*x))^(-2))),nrow=2,ncol=2,byrow = T)
  list(lnL=lnL,grad=grad,hess=hess)
}
LOGISTIC<-function(a,fun){
  t0<-a
  obj<-fun(a)
  t1=t0-solve(obj$hess,obj$grad)
    while (sqrt((t0-t1)*(t0-t1))>0.1) {
    t0<-t1
    obj<-fun(t0)
    t1<-t0-solve(obj$hess,obj$grad)
    }
  print(t1)
}

LOGISTIC(c(0.1,0.1),logistic)

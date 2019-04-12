#Lab Session 7
#Use Newton’s method to find the maximum likelihood estimate
#for the coefficients in a logistic regression. The steps are:

#1.Write down likelihood function
beta0=1
beta1=2
n<-1000
x<-1e2*runif(n,-1,1)
y<-runif(n,-1,1)
for(i in 1:n){
  if(y[i]>0){y[i]=1}
  else{y[i]=0}
}
yy<-beta0+beta1*x#P(y=1)=p,yy=g(p)

func<-function(beta){
  sum((y*yy+log(1+exp(yy))))
}

#2.Find the gradient and Hessian matrix.
grad=function(beta){
  matrix(c(sum(y-exp(yy)/(1+exp(yy))),sum(y*x-x*exp(yy)/(1+exp(yy)))),2,1)
}
hess=function(beta){
  matrix(c(-sum(exp(yy)/(1+exp(yy))^2),
           -sum(x*exp(yy)/(1+exp(yy))^2),
           -sum(x*exp(yy)/(1+exp(yy))^2),
           -sum(x^2*exp(yy)/(1+exp(yy))^2)),2,2)
}
f=expression(beta0+beta1*x)
newton.opt<-function(beta0,beta1,f,b){
  beta<-matrix(c(beta0,beta1),2,1)
  for(i in 1:10000){
    f1=eval(f)
    a=beta[1]
    b=beta[2]
    H<-hess(beta)
    cdf<-grad(beta)
    beta=beta-solve(H)%*%cdf
    beta0=beta[1]
    beta1=beta[2]
    f2=eval(f)
    c=abs(sum(f1-f2)/1e5)
    if(c<b){
      break  }}
  #return(a)
  return(c(beta,i,c))
}
newton.opt(1,2,f,10)


for(i in 1:10){print(newton.opt(i^2,i*2,f,10))}

beta0
beta1
#Lab Session 8
#Use optim() to carry out maximum likelihood for the Logistic regression model.
?optim()

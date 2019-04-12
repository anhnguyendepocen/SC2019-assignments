#Lab7

# Make the log likelihood function

func<-function(beta0,beta1){
  sum(y*(beta0+beta1*x)-log(1+exp(beta0+beta1*x)))
}

grad<-function(beta0,beta1){
  matrix(c(sum(y-exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))), 
           sum(y*x-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)))),2,1)
}

hess = function(beta0,beta1){ 
  matrix(c(sum(-exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2), 
           sum(-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2), 
           sum(-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2), 
           sum(-x^2*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2)),2,2) 
}

newton<-function(beta0,beta1,n=100,tol=0.000001){
  Beta<-as.matrix(c(beta0,beta1))
  for(i in 1:n){
    b<-grad(Beta[1],Beta[2])
    A<-hess(Beta[1],Beta[2])
    Beta<-Beta-solve(A,b)
    if(solve(A,b)[1]<tol&solve(A,b)[2]<tol){
      cat("Beta0=",Beta[1],"Beta1=",Beta[2])
      break
    }
    else
      i=i+1
  }
  return(Beta)
}

# Generate some data
beta0<-2
beta1<-2
n<-100
eta<-rlogis(n,location = 0,scale = 1)
x<-(eta-beta0)/beta1
p<-exp(eta)/(1+exp(eta))
plot(x,p,col='blue',pch=20)
y<-rbinom(n,1,p)

# The optimization
betahat<-newton(2,2)
etahat<-betahat[1]+x*betahat[2]
phat<-exp(etahat)/(1+exp(etahat))

# Comparison with glm
lm<-glm(y~x,family = binomial)
summary(lm)
(Coef<-lm$coefficients)
glmetahat<-Coef[1]+Coef[2]*x
glmphat<-exp(glmetahat)/(1+exp(glmetahat))

plot(x,y,col='blue',pch=20)
points(sort(x),phat[order(x)],col='purple',type='l',lwd=5)
points(sort(x),glmphat[order(x)],type="l",col="orange",lwd=3,lty="dashed",pch=20)

#Lab8
func2<-function(beta){
  sum(y*(beta[1]+beta[2]*x)-log(1+exp(beta[1]+beta[2]*x)))
}
optim(c(0,3),func2,method="BFGS",control=list(fnscale=-1)) #find the max

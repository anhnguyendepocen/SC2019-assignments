#The Optional problem
beta<-c(1,2)
x<-rnorm(100,0,1)
##simulate x through normal distribution 
eta<-beta[1]+beta[2]*x
p<-exp(eta)/(1+exp(eta))
y<-rbinom(100,1,p)
##calculate eta and possibility and simulate y through binomial distribution

## Make the log normal likelihood function
func = function(x,y,beta){
  -1*sum(y*(beta[1]+beta[2]*x)-log(1+exp(beta[1]+beta[2]*x)))
}
##derivation
grad = function(x,y,beta){
  matrix(c(sum(y-1/(1+exp(-beta[1]-beta[2]*x))),
           sum(y*x-x/(1+exp(-beta[1]-beta[2]*x)))),2,1)
}
##Second derivative
hess = function(x,y,beta){
  matrix(c(sum(-1*exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))^2),
           -1*sum(x*exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))^2),
           -1*sum(x*exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))^2),
           -1*sum(x*x*exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))^2)),2,2)
}
logfun<-list(func,grad,hess)
NR.op2 <- function(fun,x,y,a,b,Epsilon = 10^-10,maxiter = 1000){#Newton raphson 
  result<-c()
  fun[1]->func
  fun[2]->grad
  fun[3]->hess
  for(num in 1:1000)
  {
    n<-0
    fx_<-1
    fx<-0
    beta<-c(a+rnorm(1),b+rnorm(1))
    while ((abs(fx_-fx)>Epsilon) && n <= maxiter)#the condition to break the circle
    {
      n<-n+1
      fx_<-func(x,y,beta)
      
      beta <- beta- solve(hess(x,y,beta),grad(x,y,beta))#iteration of β
      
      fx<-func(x,y,beta)
      if (any(abs(beta)>100))#break the circle when the Beta is too big in case that fx becomes inf
      break
    }
    if (n > maxiter|any(abs(beta)>100)) {
      beta<-NULL
    } 
    if (is.null(beta)==FALSE)
    {result <-c(result,round(beta,5))}

  }
  index<-duplicated(result)
  result[!index]#cancel the duplicate value
}
#The result of beta will change due to the normal and binomial distribution  
#The result of Newton raphson in one experiment
NR.op2(logfun,x,y,1,2)
#2.37145 3.18707

#The method of logistic regression
gl<-glm(y~x,family=binomial)
summary(gl)
#2.37145 3.18707

#The method of Nelder-Mead method
optim(c(beta),x=x,y=y,func)#使用optim函数优化
# 2.371692 3.187253

#L8 the Nelder Mead method
library(animation)
NelderMead<-function(fun,num){
  n<-length(num)
  x<-list()#list
  y<-c()#vector
  stop<-100
  saveGIF({
            while(stop>10^-5){
            
                for(i in 1:n){#sort y
                  y[i]<-do.call(fun,list(num[[i]][1],num[[i]][2]))
                }
                y<-sort(y)
                for(i in 1:n){#sort x
                  for(j in 1:n){
                  if (y[j]== do.call(fun,list(num[[i]][1],num[[i]][2])))
                    x[[j]]=num[[i]]
                  }
                }
                      xsum<-x[[1]]*0
                      for(i in 1:(n-1)){
                          xsum<-xsum+x[[i]]
                        }
                        x0=xsum/(n-1)
                        
                        xr<-2*x0-x[[n]]
                        yr<-do.call(fun,list(xr[1],xr[2]))##calculate the reflected point
                        if(yr<y[n-1] & yr>=y[1])#case 1:neither best nor worst point
                        {
                          x[[n]]<-xr
                          y[n]<-yr
                        }
                        else if (yr <y[1])#case 2:xr  is the best point
                        {
                          xe=2*xr-x0
                          ye<-do.call(fun,list(xe[1],xe[2]))
                          if(ye<yr){
                            y[n]<-ye
                            x[[n]]<-xe
                          }
                          else{
                            y[n]<-yr
                            x[[n]]<-xr
                          }
                        }
                        else { #case 3: xr  is the worst point 
                          xc<-0.5*(x[[n]]+x0)
                          yc<-do.call(fun,list(xc[1],xc[2]))
                          if(y[n]<=yc){#Shrink
                            for(i in 1:n){x[[i]]=0.5*(x[[i]]+x[[1]])}
                          }
                          else{#keep xc
                            x[[n]]<-xc
                            y[n]<-yc
                          }
                        }
                xarea<-c()
                for(i in 1:(n-1)){
                  xarea<-c(xarea,x[[i]]-x[[n]])}
                stop<-0.5*abs(det(matrix(xarea,nrow = 2 )))
                num<-x
                x1<-c()
                x2<-c()
                for (i in 1:n){
                  x1<-c(x1,x[[i]][1])
                  x2<-c(x2,x[[i]][2])
                }
                a <- b <- c(-2:2)
                r <- outer(a^2, b^2, "+")
                filled.contour(a,b,r , color = terrain.colors,plot.axes = { axis(1); axis(2); polygon(x1,x2) })
            } 
    },interval=0.25)
            list(x=x,y=y)
            
}

x<-list(c(1,1),c(1,2),c(2,2))
f<-function(x,y){
  x^2+y^2
}
NelderMead(f,x)#0 0
debug(NelderMead)
undebug(NelderMead)
optim(par=c(1,2),fn=f)#0 0



#L9 rank the website
A=c(0,1,0,1,1,
    0,0,1,1,1,
    1,0,0,1,0,
    0,0,0,0,1,
    0,0,1,0,0)
m<-matrix(A, byrow=T, nrow=5)
x <-(eigen(t(m))$vectors)[,1]

p<-abs(x)/sum(abs(x))
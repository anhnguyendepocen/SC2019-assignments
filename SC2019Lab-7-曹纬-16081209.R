##Find likelihood function、gradient and hessian matrix of logistic regression
#Likelihood function
func = function(beta){
  sum(y*(beta[1]+beta[2]*x-log(1+exp(beta[1]+beta[2]*x))))
}

##Gradient matrix function
grad = function(beta){
  matrix(c(sum(y-exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))),
           sum(y*x-exp(beta[1]+beta[2]*x)*x/(1+exp(beta[1]+beta[2]*x)))),
           nrow = 2)
}

##Hessien matrix function
hess = function(beta){
  matrix(c(sum(-(exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))^2)),
           sum(-(exp(beta[1]+beta[2]*x)*x/(1+exp(beta[1]+beta[2]*x))^2)),
           sum(-(exp(beta[1]+beta[2]*x)*x/(1+exp(beta[1]+beta[2]*x))^2)),
           sum(-(exp(beta[1]+beta[2]*x)*x^2/(1+exp(beta[1]+beta[2]*x))^2))),
           nrow = 2)
}


#Write a newton function to estimate parameters
newton <- function(beta,if.func = FALSE,varname, n=10000,e=1e-10){                             
  i=1 
  if(if.func==FALSE){
    while(i<n){
      grad <- grad(beta)
      hess <- hess(beta)
      beta <- beta - solve(hess)%*%grad                           
      i=i+1
      if(grad[1]<e & grad[2]<e)                                   ##criterion to stop the loop
        break
    }
    return(beta) 
  }

  
  if(if.func==TRUE){
    for (i in i:n) {
      beta0 <- beta[1]
      beta1 <- beta[2]
      dvar <- deriv(fun,varname,hessian = T,func=T)                           ##function to caculate gradient & hessian matrix
      dx <- dvar(beta0,beta1)                                                         ##caculate gradient & hessian matrix
      d <- attributes(dx)
      gradient.m <- matrix(d$gradient,nrow = 1)
      hessian.m <- matrix(d$hessian,nrow = 2)
      beta <- beta - gradient.m%*%solve(hessian.m)                              ##caculate next(x,y)
      i=i+1
      if(gradient.m[1]<e & gradient.m[2]<e)                                   ##criterion to stop loop
        break
    }
    return(beta) 
  }
}

##Simulate some data from a logistic regression model and test
beta0 <- 1
beta1 <- 2
n <- 1000
eta <- rlogis(n) #generate eta from a logistic distribution(local=0,scale=1)
x <- (eta - beta0)/beta1  
p <- exp(eta)/(1+exp(eta))
y <- rbinom(n,1,p)#generate y from a Binomial Distribution

optimOut <- newton(c(1.1,1.3)) #find the value of beta0 and beta1
beta0Hat <- optimOut[1]
beta1Hat <- optimOut[2]
etaHat <- beta0Hat + beta1Hat*x #estimate the value of eta and p
pHat <- exp(etaHat)/(1+exp(etaHat))

lmod <- glm(p~x,family=binomial) #use glm to estimate the value of eta and p
lmod$coefficients
etaHatglm <- lmod$coefficients[1]+lmod$coefficients[2]*x
pHatglm <- exp(etaHatglm)/(1+exp(etaHatglm))



## Plot
plot(x, p, pch = 20, col = "yellow" ,cex = 0.5 , xlab = expression(eta))
points(sort(x), pHat[order(x)], type = "l", col = "red", lwd = 3)
points(sort(x), pHatglm[order(x)], type = "l",
       col = "green", lty="dashed", lwd = 1, pch = 20)
legend("bottomright",legend = c("real p","estimate p","gls p"),
       lty =c(2,1,2),col = c(7,2,3),cex = 0.8)

##Use optim() to carry out maximum likelihood for the Logistic regression model
optimOut2 <- optim(c(1.1,1.3),func)
optimOut2

## Use Newton’s method to find the maximum likelihood estimate for the coefficients in a logistic regression. The steps are:
# 1.Write down likelihood function

# 2.Find the gradient and Hessian matrix

# 3.Code these up in R
## Make the log normal likelihood function
# the lilelihood function
func = function(beta0, beta1){
  sum(y*(beta0+beta1*x-log(1+exp(beta0+beta1*x))))
}
#the gradiant matrix
grad = function(beta0,beta1){
  matrix(c(sum(y-exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))),
           sum(y*x-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)))),2,1)
}
#the hessian matrix
hess = function(beta0,beta1){
  matrix(c(sum(-exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2),
           sum(-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2),
           sum(-x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2),
           sum(-x*x*exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))^2)),2,2)
}

## The optimization
# funtion with newton method
newton <- function(beta0, beta1){
  i = 0
  beta = c(beta0, beta1)# beta(0/1)-the pending parameters
  while(i >= 0){
    re_d1 <- grad(beta[1], beta[2])# get the gradient matrix
    re_d2 <- hess(beta[1], beta[2])# get the hessian matrix
    i = i + 1# number of calculations
    beta = beta - solve(re_d2)%*%re_d1# calculate the step
    if(re_d1[1] < 0.001 & re_d1[2] < 0.001){ # set up when to break
      break
    }
  }
  return(beta)# return the result and the number of cycle
}


# 4.Simulate some data from a logistic regression model.Test your code.
## Generate some data
beta0 <- 1
beta1 <- 2
n <- 100
eta <- rlogis(n,location = 0, scale = 1)# random generation of eta
x <- (eta - beta0) / beta1# calculate x
p <- (1+exp(-eta))^(-1)# get the probability of y=1
plot(x, p, col = "blue", pch = 20)# plot random x & p 
y <- rbinom(n, 1, p)# random generation of y
## comparison with GLM
betahat <- newton(1,2)# use the function to test the initial value
etahat <- betahat[1] + x*betahat[2]
phat <- (1+exp(-etahat))^(-1)

lm <- glm(p ~ x, family = binomial)# use the glm package to estimate the value of variables
lmcoef <- lm$coefficients
etahatglm <- lmcoef[1] + x*lmcoef[2]
phatglm <- (1+exp(-etahatglm))^(-1)

plot(x, p, col = "blue", pch = 20)
points(sort(x), phat[order(x)], type = "l", col = "orange", lwd = 5)
points(sort(x), phatglm[order(x)], type = "l", col = "pink", lwd = 5, lty = "dashed", pch = 20)

## Use optim() to carry out maximum likelihood for the Logistic regression model.
func = function(beta){
  sum(y*(beta[1]+beta[2]*x-log(1+exp(beta[1]+beta[2]*x))))
}
optimfun <- optim(c(3, 5), func, method = "BFGS", control = list(fnscale = -1))# use optim function to calculate directly
optimfun$par# get the estimated parameters
optimfun$value# get the value of the optimized function
optimfun$counts# get the number of calls to fn and gr
#likelyhood function for linear model y = beta0+beta1*x+e
#first write down the likelyhood function for y:
#e ~ N(0,1),so yi ~ N(beta0+beta1*xi, sigma^2)
#logL = -n*log(sqrt(2*pi)*sigma) - sum(yi - beta0-beta1*xi)^2/2*sigma^2

#to maximize the logL, that is to minimize sum(yi-beta0-beta1*xi)^2
#these are the objective function, gradient and hessian function
logl_fun <- function(beta){
  sum((y - beta[1] - beta[2]*x)^2)
}
grad <- function(beta){
  matrix(c(sum(-2*(y - beta[1] - beta[2]*x)), sum(-2*x*(y - beta[1] - beta[2]*x))), 2, 1)
}
hess <- function(beta){
  matrix(c(2*length(x),2*sum(x),2*sum(x),2*sum(x^2)),2,2)
}

#cars is a dataset containing cars'speed and distance to stop
data(cars)
x <- cars$speed
y <- cars$dist
sigma <- 1

plot(y ~ x, data = cars, pch=20, col = "blue")

beta <- c(2,3)

#consider the deriv couldn't work out the sum, use the new function
#nr_optim function to work out the optimisation of a likelyhood function
nr_optim <- function(func, grad, hess, beta, epsilon = 1e-5, maxit = 10000){
  x <- beta
  xn <- NULL
  n <- 0
  g <- 1
  
  while(abs(g) > epsilon & n < maxit){
    g <- sqrt((grad(x)[1])^2+(grad(x)[2])^2)
    dfx <- solve(hess(x))%*%grad(x)
    xn <- x - dfx
    x <- xn
    n <- n+1
  }
  
  return(list(beta0 = x[1], beta1 = x[2]))

}
#the result of the estimator is beta0hat = -17.579, beta1hat = 3.9324
betahat <- nr_optim(logl_fun, grad, hess, beta)
be0 <- as.numeric(betahat[1])
be1 <- as.numeric(betahat[2])
yHat <- be0 + be1*x

#we could find the result of beta from ols is the same as the mle
lmod <- lm(dist ~ speed, cars)
b0 <- lmod$coefficients[1] 
b1 <- lmod$coefficients[2]
yHatOLS <- b0+b1*x
points(sort(x), yHat[order(x)], type = "l", col = "red", lwd = 10)
points(sort(x), yHatOLS[order(x)], type = "l",
       col = "blue", lty="dashed", lwd = 2, pch = 20)


#newton method for MLE to estimate the coeffients in logistic regression
#ita_i = beta0 + beta1*x_i + e,with e~N(0,sigma^2)
#logL = sum(yi*ita_i - log(1+exp(ita_i)))
# lgs_fun <- function(beta){
#   ita <- beta[1]+beta[2]*x
#   sum(y*ita - log(1+exp(ita)))
# }
# lgs_grad <- function(beta){
#   ita <- beta[1]+beta[2]*x
#   matrix(c(sum(y-(exp(ita)/(1+exp(ita)))), sum(y*x - (exp(ita)*x)/(1+exp(ita)))), 2, 1)
# }
# lgs_hess <- function(beta){
#   ita <- beta[1]+beta[2]*x
#   matrix(c(sum(-exp(ita)/(1+exp(ita))^2), sum(-x*exp(ita)/(1+exp(ita))^2), 
#            sum(-x*exp(ita)/(1+exp(ita))^2), sum(-x^2*exp(ita)/(1+exp(ita))^2)), 2, 2)
# }

func <- function(theta){
  ita <- theta[1]+theta[2]*x
  sum(y*ita - log(1+exp(ita)))
}
grad <- function(theta){
  ita <- theta[1]+theta[2]*x
  matrix(c(sum(y-exp(ita)/(1+exp(ita))), sum(x*(y - exp(ita)/(1+exp(ita))))), 2, 1)
}
hess <- function(theta){
  ita <- theta[1]+theta[2]*x
  a1 <- sum(-exp(ita)/(1+exp(ita))^2)
  a2 <- sum(-x*exp(ita)/(1+exp(ita))^2)
  a4 <- sum(-x^2*exp(ita)/(1+exp(ita))^2)
  matrix(c(a1,a2,a2,a4), 2,2)
}


#wcgs data:chd-weather have heart disease, chd-how many cigarettes a day 
lmod <- glm(chd ~ cigs, family = binomial, wcgs)
summary(lmod)

#generate a series of data for the logistic regression
y <- rbinom(30, 1, 0.3)
x <- round(runif(30, 1, 30))
lgmod <- glm(y ~ x, family = binomial)
summary(lgmod)


x <- wcgs$cigs
y <- as.numeric(wcgs$chd)-1
theta <- c(1,-2)

nr_optim(func(theta), grad(theta), hess(theta), theta)
#函数在求解过程中出现了NaN，无法继续，但我还没找出原因

#Use optim() to carry out maximum likelihood for the Logistic regression model.

fr <- function(beta){
  ita <- beta[1]+beta[2]*x
  sum(y*ita - log(1+exp(ita)))
}

optim(par = bvec, fn = fr, method = "BFGS", control = list(fnscale = -10), hessian = T)
optim(par = bvec, fn = fr, method = "L-BFGS-B", control = list(fnscale = -10))
optim(par = bvec, fn = fr, method = "Nelder-Mead", control = list(fnscale = -10))














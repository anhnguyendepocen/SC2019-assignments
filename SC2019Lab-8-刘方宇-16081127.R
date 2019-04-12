beta = c(1,1)
x = rnorm(1000,3,1)
eta = beta[1] + beta[2]*x
p = exp(eta)/(exp(eta)+1)
y = rbinom(1000, 1, p)
likeli <- function(beta){
  -sum(y*(beta[1]+beta[2]*x)-log(exp(beta[1]+beta[2]*x)+1))
}
optim(c(1.1,0.9),likeli)
optim(c(1.1,0.9),likeli,method = 'BFGS')
optim(c(1.1,0.9),likeli,method = 'L-BFGS-B')


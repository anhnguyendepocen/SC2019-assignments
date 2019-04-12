# Use generalised inverse to solve a linear regression problem
X <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12), 4, 2)
y <- 5 * X[, 1] + 6 * X[, 2] + rnorm(4)
library(MASS)
beta <- ginv(X) %*% y
beta
t(y - X %*% beta) %*% (y - X %*% beta)

# compare with the lm() function in R
lmod <- lm(y ~ X)
coef(lmod)
sum(re <- residuals(lmod)^2)
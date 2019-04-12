# Statistical Computing Homework6 
# author:Yuchao Tao   StudentID:16081045

# compute vector p of probabilities for logistic regression with logit link
fun.p <- function(X, beta) {
  X <- as.matrix(X)
  beta <- as.vector(beta)
  p <- exp(X %*% beta) / (1 + exp(X %*% beta))
  return(p)
}

# binomial log likelihood function
# input: vectors: y = counts; m = sample sizes; p = probabilities 
# output: log-likelihood l
fun.l <- function(y, m, p) {
  l <- t(y) %*% log(p) + t(m - y) %*% log(1 - p)
  return(l) }

fun.MLE <- function(X, y, m, beta1, eps1, eps2, max.loop)
{
  # Input:
  # X is n-by-(r+1) matrix 
  # y is n-by-1 vector of success counts
  # m is n-by-1 vector of sample sizes
  # beta.1 is (r+1)-by-1 vector of starting values for regression 
  
  # Iteration controlled by:
  # eps1 is absolute convergence criterion for beta
  # eps2 is absolute convergence criterion for log-likelihood
  # max.loop is the maximum allowable number of iterations
  
  # Output:
  # result is a list containing:
  # beta.MLE is the beta max likelihood estimated value
  # CD.hist is the iteration history of convergence differences
  # beta.hist is the iteration history of beta
  
  beta2 <- rep(-Inf, length(beta1)) # beta2 initial value
  diff.beta <- sqrt(sum((beta1 - beta2)^2)) # Euclidean distance
  loglike1 <- fun.l(y, m, fun.p(X, beta1)) # update loglikelihood1 
  loglike2 <- fun.l(y, m, fun.p(X, beta2)) # update loglikelihood2 
  diff.like <- abs(loglike1 - loglike2)
  # initial iteration index
  i <- 1   
  # line search step sizes, excluding 0
  alpha.step <- seq(-1, 2, by = 0.1)[-11] 
  CD.hist <- data.frame(i, diff.beta, diff.like, loglike1, loglike2, step.size = 1) # iteration history 
  beta.hist <- matrix(beta1, nrow = 1)
  while ((i <= max.loop) & (diff.beta > eps1) & (diff.like > eps2)) 
  {
    # increment iteration
    i <- i + 1
    # update beta
    beta2 <- beta1
    mu2 <- m * fun.p(X, beta2)
    # variance matrix
    var_beta2  <- diag(as.vector(m * fun.p(X, beta2) * (1 - fun.p(X, beta2))))
    increm <- solve(t(X) %*% var_beta2 %*% X, t(X) %*% (y - mu2)) # solve for increment
    # line search for improved step size
    loglike.step <- rep(NA, length(alpha.step)) # init loglike for line search 
    for (j in 1:length(alpha.step)) {
      loglike.step[j] <- fun.l(y, m, fun.p(X, beta2 + alpha.step[j] * increm))
    }
    # step size index for max increase in log-likelihood (if tie, [1] takes first)
    max.alpha.step <- which(loglike.step == max(loglike.step))[1]
    # update beta
    beta1 <- beta2 + alpha.step[max.alpha.step] * increm   
    diff.beta <- sqrt(sum((beta1 - beta2)^2)) 
    loglike2 <- loglike1               
    loglike1 <- fun.l(y, m, fun.p(X, beta1)) # update loglikelihood
    diff.like <- abs(loglike1 - loglike2) 
    # iteration history
    CD.hist <- rbind(CD.hist, c(i, diff.beta, diff.like, loglike1, loglike2, alpha.step[max.alpha.step]))
    beta.hist <- rbind(beta.hist, matrix(beta1, nrow = 1)) 
  }
  
  # output the result
  result <- list()
  result$beta.MLE <- beta1 
  result$iter <- i-1 
  result$CD.hist <- CD.hist 
  result$beta.hist <- beta.hist
  return(result) 
}

# Beetles data, conc is the CS2 concentration, y is the number of beetles killed
# n is the number of beetles exposed, rep is the Replicate number (1 or 2)
beet <- read.table("http://statacumen.com/teach/SC1/SC1_11_beetles.dat", header = TRUE)
beet$rep <- factor(beet$rep)
n <- nrow(beet)
m <- beet$n
y <- beet$y
X.temp <- beet$conc
# quadratic model
X <- matrix(c(rep(1,n), X.temp, X.temp^2), nrow = n)
colnames(X) <- c("Int", "conc", "conc2")
# number of regression coefficients - 1
r <- ncol(X) - 1  
# initial beta vector
beta1 <- c(log(sum(y) / sum(m - y)), rep(0, r))
# fit betas using our Fisher Scoring function
result <- fun.MLE(X, y, m, beta1, 10^(-6), 10^(-7), 50)
result
# fitting the glm model to exam the result is right or wrong
beet$conc2 <- beet$conc^2
beet.glm <- glm(cbind(y, n-y) ~ conc + conc2, data = beet, family = binomial)
summary(beet.glm)

#optim() function, use "BFGS"
Y <- as.matrix(beet$y)
loglikelihood_Logit_Stable <- function(beta, X, Y)
{
  return(-sum(Y*(X %*% beta - log(1 + exp(X %*% beta)))
         + (1 - Y)*(-log(1 + exp(X %*% beta)))))
}
beta <- c(log(sum(y) / sum(m - y)), rep(1, r))
optimlogit <- optim(par = beta1, fn = loglikelihood_Logit_Stable, X = X, Y = Y,
                    method = "BFGS", hessian = TRUE)
optimlogit




















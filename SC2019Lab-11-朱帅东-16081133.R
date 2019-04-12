###SC2019Lab-11-Shuaidong Zhu-16081133

##Part 1 :Use generalised inverse to solve a linear regression problem
#The function that can find generalized inverse
gen_inverse <- function(A){
  svdresult <- svd(A)
  r <- qr(A)$rank
  return(svdresult$v[,1:r] %*% solve(diag(svdresult$d[1:r])) %*% t(svdresult$u[,1:r]))
}
#Example 1 
X1 <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40), 4, 3)
y1 <- 4 * X1[, 1] - 3 * X1[, 2] + 1e-04 * (rnorm(4) - 0.5)
#Use generalised inverse
beta_hat1 <- gen_inverse(X1) %*% y1
beta_hat1

#Sum of residuals
t(y1 - X1 %*% beta_hat1) %*% (y1 - X1 %*% beta_hat1)
#y_hat
y_hat1 <- X1 %*% beta_hat1
y_hat1
#Use lm()
model1 <- lm(y1 ~ X1[,1] +  X1[,2])
summary(model1)
#solve
X3 <- X1[,1:2]
beta_hat3 <- gen_inverse(X3) %*% y1
beta_hat3

#Example 2
X2 <- matrix(c(1, 2, 4, 8, 3, 6, 9, 12, -11, -22, -32, -40), 6, 2)
y2 <- 4 * X2[, 1] - 3 * X2[, 2] + 1e-04 * (rnorm(6) - 0.5)
#Use generalised inverse
beta_hat2 <- gen_inverse(X2) %*% y2
beta_hat2
#Sum of residuals
t(y2 - X2 %*% beta_hat2) %*% (y2 - X2 %*% beta_hat2)
#y_hat
y_hat2 <- X2 %*% beta_hat2 
y_hat2
#Use lm()
model2 <- lm(y2 ~ X2[,1] +  X2[,2])
summary(model2)

###Part 2 : Use The Power Method to redo the google pagerank problem
##The Power Method function  
Power_method <- function(A,initial_x,k = 1000){
    q_k <- initial_x/(sqrt(sum(initial_x^2)))
    i <- 0
    while(i <= k){
      i <- i + 1
      x_k <- A %*% q_k
      q_k <- x_k/(sqrt(sum(x_k^2)))
    }
    lammda <- t(q_k) %*% A %*% q_k
    return(list(eigenvalue = lammda,eigenvector = q_k))
}

Page_m <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5,5)
initial_x <- c(1,2,3,4,5)
power_result <- Power_method(Page_m,initial_x,k = 100)
power_result
#Compare with eigen function
eigen(Page_m)
#Find the weight
rscale <- power_result$eigenvector/sum(power_result$eigenvector)
rscale
#The greater the weight, the greater the importance,get the pagerank
names(rscale) <- c("Webpage 1","Webpage 2","Webpage 3","Webpage 4","Webpage 5")
rank(rscale)

###Part 3 :Code up QR Algorithm
QRfenjie <- function(A, k = 1000){
  i <- 0
  initial_qr <- qr(A)
  Q0 <- qr.Q(initial_qr)  
  Q <- Q0
  while (i <= k) {
       i <- i + 1
       qrresult <- qr(A)
       R_k <- qr.R(qrresult)
       Q_k <- qr.Q(qrresult)
       A <- R_k %*% Q_k
       Q <- Q %*% Q_k
  }       
  Q <- solve(Q0) %*% Q
  return(list(Q = Q,S = A))
}
#Example
A <- matrix(c(1,2,3,4),2,2,byrow = T)
QRfenjie(A)
#Compare with eigen function
eigen(A)

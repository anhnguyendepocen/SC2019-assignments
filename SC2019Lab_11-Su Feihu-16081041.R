###SC2019Lab_11-Su Feihu-16081041
###Name:Su Feihu   StudentID:16081041


##QR algorithm
QR.algorithm <- function(A, maxiter = 10000)
{
  #A is the matrix to decomposite
  #maxiter is the maxinum of iteration.
  iter <- 0
  while (!all(A[lower.tri(A)] == 0) & iter <= maxiter)
    #the matrix is not a upper triangular matrix and the number of iteration is smaller than maxiter.
  {
    qr.A <- qr(A)       #QR decomposition   A = Q*R
    Q <- qr.Q(qr.A)     #Q
    R <- qr.R(qr.A)     #R
    A <- R %*% Q        #A := R*Q
    iter <- iter + 1
  }
  
  if (iter > maxiter) {stop("it is not convergent.")}
  
  S <- A                #S := A
  eigen.val <- diag(S)  #eigenvalue is the Diagonal elements
  
  result <- list(S = S, eigenvalue = eigen.val, iteration = iter)
  
  return(result)
}

A <- matrix(c(1, 3, 2, 4), 2, 2)
QR.algorithm(A)
#$S
#[,1]       [,2]
#[1,] 5.372281 -1.0000000
#[2,] 0.000000 -0.3722813

#$eigenvalue
#[1]  5.3722813 -0.3722813

#$iteration
#[1] 280

B <- matrix(c(2, 4, 1, 5, 2, 7, 3, 5, 4), 3, 3)
QR.algorithm(B)
#$S
#[,1]      [,2]       [,3]
#[1,] 11.15692  1.954503  1.6632532
#[2,]  0.00000 -3.874123 -1.5535470
#[3,]  0.00000  0.000000  0.7172063

#$eigenvalue
#[1] 11.1569166 -3.8741229  0.7172063

#$iteration
#[1] 706


##Power Method
power.method <- function(A, x0, maxiter = 10000)
{
  ##A is the matrix
  ##x0 is the initial vector
  #maxiter is the maxinum of iteration.
  q <- x0 / c(sqrt(t(x0) %*% x0))    #Set initial vector q0 = x0/(||x0||)
  iter <- 0
  while (iter < maxiter)
  {
    xk <- A %*% q                    #compute x(k) = A*q(k-1)
    q <- xk / c(sqrt(t(xk) %*% xk))  #Normalise 
    iter <- iter + 1
  }
  eig.val <- t(q) %*% A %*% q        #Estimate 
  
  result <- list(eigenvalue = eig.val, eigenvector = q)   #result 
  
  return(result)
}

A <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0), 5, 5)

x0 <- rep(1/n,n)    #initial vector
power.method(t(A), x0)
A.rank <- power.method(t(A), x0)
A.rank <- A.rank$eigenvector/sum(A.rank$eigenvector)
rank(A.rank)
-rank(A.rank) + length(A.rank) + 1

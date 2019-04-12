#Q-R algorithm in R
#input a n*n matrix A,then use the QR decomposition find the upper triangular matrix S
#whose diagnoal is the eigenvalue 

qr_algorithm <- function(x){
  counts = 0
  Ai <- x
  
  while (counts < 1000) {
    Q <- qr.Q(qr(Ai))
    R <- qr.R(qr(Ai))
    
    Aj = R %*% Q
    Ai = Aj
    counts = counts + 1
  }
  s = Aj
  l = ncol(s)
  e.value <- diag(s)
  
  res = list(uppertri_matrix = s, eigenvalue = e.value)
  
  return(res)
}

A = matrix(1:4, 2, 2, byrow = T)
qr_algorithm(A)
#compare the result with the function eigen()
eigen(A)

x = matrix(c(2,1,1,3,4,-3,2,2,1), 3,3)
eigen(x)
qr_algorithm(x)

#use the power method to rewrite the pagerank function
A = matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0), 5, 5)
a = t(a)
#function to get the normalization of vector x
normal_vector <- function(x){
  s = sqrt(sum(x^2))
  x = x / s
  x
}

#input the n*n page matrix x
page.rank <- function(x){
  m = nrow(x)
  xtemp <- rep(1,m)/m#set the initial vector x0
  
  k = 0
  while (k < 1000) {
    q <- matrix(normal_vector(xtemp), ncol = 1)
    xtemp = x %*% q
    k = k + 1
  }
  lambda = t(q) %*% x %*% q
  p.rank = xtemp/sum(xtemp)
  p.order = order(pagerank, decreasing = T)
  list(eigenvalue = lambda, eigenvector = xtemp, pagerank = p.rank, order = p.order)
  
}

page.rank(a)
#equal to the result with the function eigen()


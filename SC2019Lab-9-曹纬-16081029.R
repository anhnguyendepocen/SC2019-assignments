pageign<- function(A){
  eigenanalysis <- eigen(t(A))
  values <- eigenanalysis$values
  vectors <- eigenanalysis$vector
  return(list(eigenvalues=values,eigenvectors=vectors))
}

A <- matrix(c(0,1,0,1,1
              ,0,0,1,1,1
              ,1,0,0,1,0
              ,0,0,0,0,1
              ,0,0,1,0,0),nrow = 5 , byrow = TRUE)

e <- pageign(A)

pagerank <- function(x){
  k <- 1/sum(x)
  realrank <- x*k
  p.order <- order(realrank,decreasing = TRUE)
  return(list(order=p.order))
}
pagerank(d)
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
rank(x2, ties.method= "last")

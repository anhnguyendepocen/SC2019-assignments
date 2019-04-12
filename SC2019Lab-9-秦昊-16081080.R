#find out the pagerank for the five webpages
#first we have the connectivity matrix A
A = matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0), 5, 5)
#r = a*t(A)*r calculate the eigenvector r
(w = eigen(t(A)))

page.vector <- Mod(w$vectors)

#now scale the pagerank r, r = (r1,r2,r3,r4,r5)'
#and sum(ri) = 1
p.matrix <- matrix(1:25,5,5)
for (i in 1:5) {
  p.matrix[, i]  = page.vector[,i]/sum(page.vector[,i])
}  

#calculate the sqrt for the sum of square of vector
sumsquare <- function(x){
  l = length(x)
  s = 0
  for (i in 1:l) {
    s = s + x[i]^2
  }
  sqrt(s)
}
#pagerank - p.rank
p.rank <- 1:5
for (i in 1:5) {
  p.rank[i] <- sumsquare(p.matrix[i, ])
}
p.rank <- p.rank/sqrt(5)#to make sum of ri equal to 1
#output the pagerank number and the order for them
(result <- list(pagerank = p.rank, order = order(p.rank, decreasing = T)))

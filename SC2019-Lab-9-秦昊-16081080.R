#find out the pagerank for the five webpages
#first we have the connectivity matrix A
A = matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0), 5, 5)
#r = a*t(A)*r calculate the eigenvector r
(w = eigen(t(A)))

#select the positive eigenvalue and corresponding eigenvector
page.vector <- w$vectors[,1]
page.value <- w$values[1]

page.value <- Mod(page.value)
page.vector <- Mod(page.vector)
#scale the pagerank to let sum(ri) = 1
pagerank <- page.vector/sum(page.vector)
#output the result and the order of pagerank
(result <- list(pagerank = pagerank, order = order(pagerank, decreasing = T)))

#I use the second method to figure out the pagerank
#the probability matrix
p.matrix <- matrix(1:25,5,5)
b <- t(A)
for (i in 1:5) {
  p.matrix[, i] = b[, i]/sum(b[, i])
}
g <- 0.85*p.matrix + 0.15/5

pr <- matrix(rep(1, 5),ncol = 1)
count <- 0
eps <- 0

#from the iteration to get possibility of page
while (eps == 0) {
  temp = pr
  pr = g%*%temp
  
  count = count + 1
  ep = abs(pr - temp)
  l = length(ep)
  flag = rep(1,l)
  for (i in 1:l) {
    if(ep[i] > 1e-9){
      flag[i] = 0
    }
  }
  
  if(0%in%flag)
    eps = 0
  else
    eps = 1

}
temp

pr2 <- temp/sum(temp)
(result <- list(pagerank = pr2, order = order(pr2, decreasing = T)))

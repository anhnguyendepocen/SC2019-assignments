##Part 1##################################
# Use generalised inverse to solve a linear regression problem, 
# and compare with the lm() function

library(faraway)
head(swiss)  #get a data set
attach(swiss)
fit.lm <- lm(Fertility~Education+Catholic) 
summary(fit.lm)$coefficient
yhat1 <- fitted.values(fit.lm)

x <- cbind(rep(1,nrow(swiss)),as.matrix(swiss[,c(4,5)],ncol = 2))#construct the coefficient matrix
y <- swiss[,1]
library(MASS)
#estimate by pseudo-inverse
beta <- ginv(x)%*%y;beta
residual <- t(y - x %*% beta) %*% (y - x %*% beta)
yhat2 <- x %*% beta

#compare fitting effect with plot
plot(Fertility~Education)
points(Education,yhat1,col = "red")
points(Education,yhat2,col = "green",pch = 5)
legend("topright",c("raw data","lm.fit","generalize inverse"),col = c("black","red","green"),pch = c(1,1,5))


##Part 2##################################

#Use QR decomposition to write your own svd function
# method of QR.decomposition
QR.decomposition <- function(A){
  Qlist <- diag(1,nrow = nrow(A))
  for (i in 1:100) {
    Q <- qr.Q(qr(A))
    R <- qr.R(qr(A))
    Qlist <- Qlist%*%Q
    A <- R%*%Q
  }
  S <- Q%*%R
  return(list(S = S,eigenvectors = Qlist,eigenvalues = diag(S)))
}
A <- matrix(c(1,3,2,4))
QR.decomposition(A)
eigen(A)#compare QR method with eigen() 

##Part 3##################################

#use The Power Method to redo your google pagerank problem

powermethod <- function(x){
  #use the power method to calculate the dominant eigenvalue for matrix
  n <- nrow(x)
  a <- rnorm(n)
  for (i in 1:100) {
    a <- x%*%a
    a <- a/(sqrt(sum(a*a)))
  }
  lamda <- t(a)%*%x%*%a
  return(list(eigenvector = a,eigenvalue = lamda))
}

#Google's pagerank matrix
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0), 5)
At <- t(A)
rankvector<-powermethod(At)
rankscale <- rankvector/sum(rankvector)
names(rankscale) <- c("page 1","page 2","page 3","page 4","page 5")
sort(rankscale)
##result : 21345 (sort from small to large,the same as that calculate by function eigen())


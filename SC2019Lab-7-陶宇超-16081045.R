# Statistical Computing Homework8
# author:Yuchao Tao   StudentID:16081045

#homework7 NelderMead method
rm(list = ls())
setwd("/Users/tyc_219/Desktop/R programs")
png("NelderMead%03d.png")
library(animation)
NelderMead <- function(X, fun, alpha = 1, gamma = 2, lo = 0.5, 
                       mu = 0.5, eps = 10^(-8), max.loop = 50)
{
  i <- 1
  #build a empty vector to store iterative triangle area 
  area.hist <- vector(mode = "numeric", length = 0)
  area.hist[1] <- 1
  #eps is the absolute convergence criterion for triangle area
  while((i <= max.loop) & (area.hist[i] > eps))
  {
    i <- i + 1
    x1 <- X[1,1]
    x2 <- X[1,2]
    fx.1 <- eval(fun)
    x1 <- X[2,1]
    x2 <- X[2,2]
    fx.2 <- eval(fun)
    x1 <- X[3,1]
    x2 <- X[3,2]
    fx.3 <- eval(fun)
    FX <- c(fx.1, fx.2, fx.3)
    x.x <- X[,1]
    y.y <- X[,2]
    plot(X[,1], X[,2], xlim = c(-1,2), ylim = c(-1,2), xlab = "x", ylab = "y", 
         main = "Nelder-Mead Iteration", pch = 20, cex = 0.5)
    lines(rbind(c(X[1,1],X[1,2]), c(X[2,1],X[2,2]), c(X[3,1],X[3,2]), c(X[1,1],X[1,2])))
    #Get the index of the maximum and minimum、median values in FX
    maximum <- match(max(FX), FX)
    median <- match(median(FX), FX)
    minimum <- match(min(FX), FX)
    #X0: Centriod of the remaining n points
    X0 <- c(sum(X[1,1]+X[2,1]+X[3,1])/3, sum(X[1,2]+X[2,2]+X[3,2])/3)
    #Xr: Reflected point
    Xr <- c(X0[1]+alpha*(X0[1]-X[maximum,1]), X0[2]+alpha*(X0[2]-X[maximum,2]))
    x1 <- Xr[1]
    x2 <- Xr[2]
    fx.xr <- eval(fun)
    
    if((FX[minimum] <= fx.xr) && (fx.xr < FX[maximum]))
    {
      X[maximum,1] <- Xr[1]
      X[maximum,2] <- Xr[2]
      p <- matrix(c(X[1,1],X[1,2],1,X[2,1],X[2,2],1,X[3,1],X[3,2],1), nrow = 3, byrow = TRUE)
      area.hist[i] <- abs(0.5*det(p))
    }
    
    else if(fx.xr < FX[minimum])
    {
      Xe <- c(X0[1]+gamma*(Xr[1]-X0[1]), X0[2]+gamma*(Xr[2]-X0[2]))
      x1 <- Xe[1]
      x2 <- Xe[2]
      if(eval(fun) < fx.xr){
        X[maximum,1] <- Xe[1]
        X[maximum,2] <- Xe[2]
        p <- matrix(c(X[1,1],X[1,2],1,X[2,1],X[2,2],1,X[3,1],X[3,2],1), nrow = 3, byrow = TRUE)
        area.hist[i] <- abs(0.5*det(p))
      }
      else{
        X[maximum,1] <- Xr[1]
        X[maximum,2] <- Xr[2]
        p <- matrix(c(X[1,1],X[1,2],1,X[2,1],X[2,2],1,X[3,1],X[3,2],1), nrow = 3, byrow = TRUE)
        area.hist[i] <- abs(0.5*det(p))
      }
    }
    
    else if(fx.xr >= FX[maximum])
    {
      Xc <- c(X0[1]+lo*(X[maximum,1]-X0[1]), X0[2]+lo*(X[maximum,2]-X0[2]))
      x1 <- Xc[1]
      x2 <- Xc[2]
      if(eval(fun) < FX[maximum]){
        X[maximum,1] <- Xc[1]
        X[maximum,2] <- Xc[2]
        p <- matrix(c(X[1,1],X[1,2],1,X[2,1],X[2,2],1,X[3,1],X[3,2],1), nrow = 3, byrow = TRUE)
        area.hist[i] <- abs(0.5*det(p))
      }
      else{
        X[maximum,1] <- X[minimum,1]+mu*(X[maximum,1]-X[minimum,1])
        X[maximum,2] <- X[minimum,2]+mu*(X[maximum,2]-X[minimum,2])
        X[median,1] <- X[minimum,1]+mu*(X[median,1]-X[minimum,1])
        X[median,2] <- X[minimum,2]+mu*(X[median,2]-X[minimum,2])
        p <- matrix(c(X[1,1],X[1,2],1,X[2,1],X[2,2],1,X[3,1],X[3,2],1), nrow = 3, byrow = TRUE)
        area.hist <- abs(0.5*det(p))
      }
    }
    
  }
  
  return(list(Point_coordinates = X, Area = area.hist, Iteration = i))
}

#by the result, we can find the different initial value lead to different convergence result
X <- matrix(c(1,1,2,1,2,2), nrow = 3)
out <- NelderMead(X, expression(x1^2 + x2^2))
dev.off()
out
bm.files <- sprintf("NelderMead%03d.png", 1:25)
im.convert(files = bm.files, output = "NelserMead.gif")

X <- matrix(c(2,3,2,1,4,6), nrow = 3)
NelderMead(X, expression(x1^2 + x2^2))
X <- matrix(c(0.1,0.15,0.2,0.2,0.15,0.15), nrow = 3)
NelderMead(X, expression(x1^2 + x2^2))


#homework8 Google pagerank
#Transformation probability matrix
probabilityMatrix <- function(G){
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  A <- matrix(0,nrow(G),ncol(G))
  for (i in 1:n) A[i,] <- A[i,] + G[i,]/cs
  A
}

#Recursively computing matrix eigenvalues
eigenMatrix <- function(G,iter=100){
  iter <- 10
  n <- nrow(G)
  x <- rep(1,n)
  for (i in 1:iter) x <- G %*% x
  x/sum(x)
}

A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)
A
G <- probabilityMatrix(A);G
#Q is the important weight of these pages, sum of Q is 1
Q <- eigenMatrix(G,100);Q
rank(Q)
sum(Q)






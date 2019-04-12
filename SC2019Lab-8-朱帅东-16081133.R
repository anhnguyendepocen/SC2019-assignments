###SC2019Lab-6-Shuaidong Zhu-16081133

setwd("G:/大三下学期/统计计算/Lab-8")

library(animation)

#Nelder Mead function
Nelder_Mead <- function(x,fun,alpha = 1,gamma = 2,rho = 0.5,sigma = 0.5,is.min = TRUE,is.plot = TRUE,tol = 1e-5, maxiter = 10000){
  n <- length(x)
  initial_x <- x
  numiter <- 0
  ani.record(reset = TRUE)# clear history before recording
  while(numiter < maxiter)
  {
    numiter <- numiter + 1
    x0 <- vector(mode="numeric",length=n-1)
    simplex <- sapply(x,FUN = fun) #Evaluate Function
    points_position <- order(simplex) #Order
    
    if(n==3 & is.plot)
    {
      xvalue <- NULL
      yvalue <- NULL
      for(i in 1:3)
      {
        xvalue <- c(xvalue,x[[i]][1])
        yvalue <- c(yvalue,x[[i]][2])
      }
      z <- matrix(c(18,13,10,9,10,13,18,
                    13,8,5,4,5,8,13,
                    10,5,2,1,2,5,10,
                    9,4,1,0,1,4,9,
                    10,5,2,1,2,5,10,
                    13,8,5,4,5,8,13,
                    18,13,10,9,10,13,18),
                  7,7)      
      filled.contour(c(-3:3),c(-3:3),z, plot.axes = {axis(1); axis(2); polygon(xvalue,yvalue)}) #Generating contour map
      ani.record()  # record the current frame
    }

    if(is.min)
    {
      worx_position <- points_position[n]
      besx_position <- points_position[1]
      X <- NULL
      for(i in (1:(n-1)))
      {
        X <- cbind(X,as.matrix(x[-worx_position][[i]]-x[[worx_position]]))
      }
      s_volume <- det(X)
      if(abs(s_volume)/2 < tol) break
      
      for (i in 1:(n-1)) {
         x0 <- x0 + x[-worx_position][[i]]
       }
      x0 <- x0/(n-1)  #Centroid
      xr <- x0 + alpha*(x0 - x[[worx_position]]) #reflected point
      fxr <- fun(xr)
      if(fxr >= min(simplex) & fxr <= max(simplex)) #case 1
        {
          x[[worx_position]] <- xr  #Form a new simplex with the reflected point
        }
      else if(fxr < min(simplex)){  #case 2
      xe <- x0 + gamma*(xr - x0)   #expansion point
      if(fun(xe) < fxr)  x[[worx_position]] <- xe #Form a new simplex with the expansion point
      else               x[[worx_position]] <- xr #Form a new simplex with the reflection point.
        }
      else if(fxr > max(simplex)){  #case 3
          xc <- x0 + rho*(x[[worx_position]]-x0) #the contraction point
          if(fun(xc) < max(simplex)) x[[worx_position]] <- xc #A new simplex is formed with the contraction point
          else { 
              for (i in 1:n) {
                  x[[i]] <- x[[besx_position]] + sigma*(x[[i]]-x[[besx_position]]) #Shrink
                   } 
               }
            }
    }
  }
  oopts = ani.options(interval = 0.5)
  ani.replay()
  saveGIF(ani.replay(), movie.name = "record_plot.gif")  
  
  return(list(numiter = numiter,bestf = min(simplex),bestx = x))
}


initial_value <- list(c(1,1),c(1,2),c(2,2))
f1 <- function(x){
  y <- x[1]^2+x[2]^2
  return(y)
}

#invoking function
Nelder_Mead(initial_value,f1)
# $numiter
# [1] 31
# 
# $bestf
# [1] 0
# 
# $bestx
# $bestx[[1]]
# [1]  0.001367367 -0.003098200
# 
# $bestx[[2]]
# [1] 0 0
# 
# $bestx[[3]]
# [1]  0.005457050 -0.001205413


###compute pageranks

A <- matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5,5)
A
#       [,1] [,2] [,3] [,4] [,5]
# [1,]    0    0    1    0    0
# [2,]    1    0    0    0    0
# [3,]    0    1    0    0    1
# [4,]    1    1    1    0    0
# [5,]    1    1    0    1    0
alpha_1 <- eigen(A)$values
r <- eigen(A)$vector
#  According to the hypothesis, alpha is greater than 0, 
#  so the first eigenvalue and the first eigenvector are selected.
rscale <- r[,1]/sum(r[,1])
finalr <- Mod(rscale)
finalr
#[1] 0.1355298 0.0816788 0.2248849 0.2664334 0.2914731
names(finalr) <- c("Webpage 1","Webpage 2","Webpage 3","Webpage 4","Webpage 5")
#The greater the weight, the greater the importance, the higher the ranking (i.e. descending order)
rank(-finalr)
# Webpage 1 Webpage 2 Webpage 3 Webpage 4 Webpage 5 
#    4         5         3         2         1 
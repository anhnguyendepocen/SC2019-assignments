
##Nelder Mead

#Find the minimum of the function f(x)=x^2+y^2
#Use a triangle with vertices (1,1),(1,2),(2,2) as the starting simplex

NelderMead <- function(fun,x,e){
  #fun : the function  
  #x : martrix consisting of initial vertices
  #e : stopping point
  
  vertex.set = list(x)
  n = ncol(x)
  volume = 1
  while(volume >= e){
    #order the function f(xi),find the worst point
    fx = NULL
    for (i in 1:n) {
      fx = c(fx,assign(paste("f",i,sep = ""),do.call(f,as.list(x[,i]))))
    }
    worstindex = order(fx)[n]
    bestindex = order(fx)[1]
    nindex = order(fx)[n-1]
    
    #calculate the centroid and reflected point 
    centroid = c(mean(x[1,-worstindex]),mean(x[2,-worstindex]))
    reflected = 2*centroid-x[,worstindex]
    fx.ref = do.call(f,as.list(reflected))
    
    #replace the worst point with a new and better point
    if (fx[bestindex] <= fx.ref & fx.ref < fx[nindex]){
      x = cbind(x[,-worstindex],reflected)
    }
    else if (fx.ref <= fx[bestindex]){
      expand = 2*reflected-centroid
      if (do.call(f,as.list(expand)) < fx.ref){
        x = cbind(x[,-worstindex],expand)
      }
      else
        x = cbind(x[,-worstindex],reflected)
    }
    else if (fx.ref >= fx[nindex]){
      contracted = 0.5*(centroid+x[,worstindex])
      if (do.call(f,as.list(contracted)) < fx[worstindex]){
        x = cbind(x[,-worstindex],contracted)
      }
      else {
        for (i in 1:ncol(x)) {
          x[,i] <- 0.5*(x[,i]+x[,bestindex])
        }
      }
    }
    
    #calculate the volume for simplex
    volume = 0.5*abs(det(rbind(x,rep(1,n))))
    vertex.set = c(vertex.set,list(x))
  }
  return(list(vertex.set=vertex.set,minimum=x))
}

f <- function(x,y){
  x^2+y^2
}

X <- matrix(c(1,1,1,2,2,2),2)

p <- NelderMead(f,X,1e-8)
p$minimum
set <- p$vertex.set

##show the iterative process
x <- seq(-5,5,by = 0.1)
y <- seq(-5,5,by = 0.1)
fx <- outer(x^2,y^2,FUN = "+")
par(mai=c(0.5,0.5,1,1))
filled.contour(x,y,fx)
par(new = TRUE,mai=c(0.5,0.5,1,1.3))

library(animation)
saveGIF({
  for (i in 1:length(set)) {
    plot(t(set[[i]]),xlim = c(-5,5),ylim = c(-5,5),xlab = "x",ylab = "y",axes = FALSE,cex=0.5)
    polygon(t(set[[i]]))
  }
},movie.name='Nelder_Mead.gif',interval=0.3,ani.width=1200,ani.height=750)



##############################

##Google's pagerank

A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0), 5)
At <- t(A)

#select the eigenvector corresponding to positive eigenvalue
rankvector <- eigen(At)$vector[,1]
rankscale <- Mod(rankvector/sum(rankvector))
names(rankscale) <- c("page 1","page 2","page 3","page 4","page 5")
sort(rankscale)
##result : 21345 (sort from small to large)

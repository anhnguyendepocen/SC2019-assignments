
#############compare
Nelder.mead.plot <- function(f,start.bvertices,tol = 1e-6, plot = T){
  N <- 1  #calculation times
  n <- nrow(start.bvertices)
  bvertices <- start.bvertices
  bvalues <- rep(0, n)
 
  start.vertices <- start.bvertices
  
  vertices <- start.vertices
  values <- rep(0, n)
  #evaluate function
  if(plot){
    x <- seq(-5,5,length = 100)
    y <- seq(-5,5,length = 100)
    z <- outer(x,y,function(a,b){apply(cbind(a,b), 1, f)})
    filled.contour(x,y,z, xlim = c(-5,5), ylim = c(-5,5), color = terrain.colors, main = paste('Nelder.mead:', 'times = ',N, seq=""), plot.axes = { axis(1); axis(2);lines(rbind(vertices,vertices[1,]), col = "blue")})
    #lines(rbind(bvertices,bvertices[1,])-cbind(c(1.1,1.1,1.1,1.1),c(0,0,0,0)), col = "blue")
  }
  
  
  for(i in 1:n){
    bvalues[i] <- f(bvertices[i,])
  }
  bvolume <- 1
  
  for(i in 1:n){
    values[i] <- f(vertices[i,])
  }
  volume <- 1
  while (abs(bvolume) > tol&&abs(volume) > tol) {
    bvertices <- bvertices[order(bvalues),]
    bvalues <- sort(bvalues)
    
    
    #bcentroid
    bcentroid <- apply(bvertices[-n,], 2, mean)
    
    #breflected piont
    breflected <- 2*bcentroid - bvertices[n,]
    brefl.value <- f(breflected)
    
    #three cases
    #1. breflected is neither best nor worst point
    if(brefl.value >= bvalues[1] && brefl.value <= bvalues[n-1]){
      bvalues[n] <- brefl.value
      bvertices[n,] <- breflected
    }
    #2. breflected is the best point
    else if(brefl.value < bvalues[1]){
      bexpand <- 2*breflected - bcentroid
      bexpand.value <- f(bexpand)
      if(bexpand.value < brefl.value){
        bprevious.value <- bexpand.value
        bprecious <- bexpand
        k = 2
        while(bexpand.value <= bprevious.value){
          bprevious.value <- bexpand.value
          bprecious <- bexpand
          
          bexpand <- breflected + k*(breflected- bcentroid)
          bexpand.value <- f(bexpand)
          k = k + 1
          
        }
        
        bvalues[n] <- bprevious.value
        bvertices[n,] <- bprecious
      }
      else{
        bvalues[n] <- brefl.value
        bvertices[n,] <- breflected
      }
      }
    #3. breflected is the worst point
      else{
        contracted <- 0.5*bcentroid + 0.5*bvertices[n,]
        contr.value <- f(contracted)
        if(contr.value >= bvalues[n]){  #shrink
          for(j in 2:n){
            bvertices[i] <- 0.5*(bvertices[1] + bvertices[i])
            bvalues[i] <- f(bvertices[i])
          }
        }
        else{
          bvalues[n] <- contr.value
          bvertices[n,] <- contracted
        }
      }
      bvolume <- 0.5*(det(bvertices[-n,] - matrix(rep(bvertices[n,],n-1),n-1)))
      N <- N + 1
      # if(plot){
      #   # filled.contour(x,y,z, xlim = c(-5,5), ylim = c(-5,5), color = terrain.colors, main = paste('Nelder.mead:', 'times = ',N, seq=""))
      #   # lines(rbind(bvertices,bvertices[1,])-cbind(c(1.1,1.1,1.1,1.1),c(0,0,0,0)))
      # 
      #   
      # }
      
      
    
    
    
    
    
    ####normal method
      vertices <- vertices[order(values),]
      values <- sort(values)
      
      
      #centroid
      centroid <- apply(vertices[-n,], 2, mean)
      
      #reflected piont
      reflected <- 2*centroid - vertices[n,]
      refl.value <- f(reflected)
      
      #three cases
      #1. reflected is neither best nor worst point
      if(refl.value >= values[1] && refl.value <= values[n-1]){
        values[n] <- refl.value
        vertices[n,] <- reflected
      }
      #2. reflected is the best point
      else if(refl.value < values[1]){
        expand <- 2*reflected - centroid
        expand.value <- f(expand)
        if(expand.value < refl.value){
          values[n] <- expand.value
          vertices[n,] <- expand
        }
        else{
          values[n] <- refl.value
          vertices[n,] <- reflected
        }
      }
      #3. reflected is the worst point
      else{
        contracted <- 0.5*centroid + 0.5*vertices[n,]
        contr.value <- f(contracted)
        if(contr.value >= values[n]){  #shrink
          for(j in 2:n){
            vertices[i] <- 0.5*(vertices[1] + vertices[i])
            values[i] <- f(vertices[i])
          }
        }
        else{
          values[n] <- contr.value
          vertices[n,] <- contracted
        }
      }
      volume <- 0.5*(det(vertices[-n,] - matrix(rep(vertices[n,],n-1),n-1)))
    
      if(plot){
        filled.contour(x,y,z, xlim = c(-5,5), ylim = c(-5,5), color = terrain.colors, main = paste('Nelder.mead:', 'times = ',N, seq=""), plot.axes = { axis(1); axis(2);lines(rbind(bvertices,bvertices[1,]));lines(rbind(vertices,vertices[1,]),col = "red")})
        #lines(rbind(vertices,vertices[1,])-cbind(c(1.1,1.1,1.1,1.1),c(0,0,0,0)), col = "red")
      }

  
    }
}
  
  
  
f <- function(x){
  y <- x[1]^2 + x[2]^2
  return(y)
}
x1 <- c(4,3)
x2 <- c(3,4.5)
x3 <- c(3, 3)
X <- rbind(x1,x2,x3)
Nelder.mead2(f,X, plot = F)  #furthest expand point
Nelder.mead(f,X, plot = F)  #general 

library(animation)
#red : general method   black:furthest expand point
saveGIF(Nelder.mead.plot(f,X, plot = T),ani.width = 600,ani.height = 600,interval = 2)


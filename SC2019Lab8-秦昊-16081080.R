#find the minimun of the function y = x1^2+x2^2 using Neld-Mead method
#with the starting vertices(1,1),(1,2),(2,2)

#fr - objective function to optim, verlist - starting vertices list
#the NM.optim() optimize the minimum by default
NM.optim <- function(fr, verlist, epsilon = 1e-9){
  m <- length(verlist)#m = n+1
  n <- m -1
  flag <- 10
  count <- 0#record the iteration times
  
  ver.matrix <- sapply(verlist, as.matrix)#form a matrix consists of vectors of vertices
  #points contain the corordinates of the vertices in every iteration
  simplex.list <- list(points = NULL)
  
  while (flag > epsilon) {
    fx <- sapply(verlist, fr)#samply() return a vector containing the values of vertices
    
    ofx <- sort(fx)
    f.order <- order(ofx)#the index of the ordered value of function
    w <- which.max(fx)#the index of the worst function
    b <- f.order[1]#the index of the min
    
    #calculate the centroid of the rest points and the reflected points
    x0 <- colMeans(ver.matrix[,-w])
    xr <- x0 + (x0 - ver.matrix[,w])
    
    if(fr(xr) >= ofx[1] & fr(xr) < ofx[n])
      ver.matrix[,w] <- xr
    else if(fr(xr)<ofx[1]){
      xe <- x0 + 2*(xr - x0)
      if(fr(xe) < fr(xr)){
        ver.matrix[,w] <- xe
      }else{
        ver.matrix[,w] <- xr
      }
    }else{
      xc <- x0 + 0.5*(ver.matrix[,w] - x0)
      if(fr(xc) < ofx[m]) ver.matrix[,w] <- xc
      else{
        for (i in 1:m) {
          ver.matrix[,i] <- ver.matrix[,b] + 0.5*(ver.matrix[,i] - ver.matrix[,b])
        }
      }
    }
    count = count + 1
    temp <- matrix(rep(1,m), 1, m)
    com.matrix <- rbind(ver.matrix, temp)
    flag <- abs(det(com.matrix))/2#flag calculate the volume of the simplex
    
    
    for (i in 1:m) {
      verlist[[i]] <- as.vector(ver.matrix[,i])
    }
    
    simplex.list[[count]] <- ver.matrix
    
  }

  min.fx <- min(sapply(verlist, fr))
  #return a list containing the ending vertices and the min of the function
  return(list(result.vertices = verlist, min.value = min.fx, it_times = count,simplex = simplex.list))
  
}

obj.fun <- function(x){
  x[[1]]^2 + x[[2]]^2
}
d <- c(1,1)
e <- c(1,2)
f <- c(2,2)

vertex <- list(d,e,f)

res = NM.optim(obj.fun, vertex)


#have a glance at the graphics of the function y = x1^2+x2^2
x = seq(-5,5,length.out = 100)
num <- length(x)
z <- matrix(rep(0,num^2), num,num)
for (i in 1:num) {
  for (j in 1:num) {
    z[i,j] <- x[i]^2+x[j]^2
  }
}
contour(x, x, z, xlim = c(-5,5), ylim = c(-5,5))

filled.contour(x, x, z, color.palette = terrain.colors)

#use this loop to show the iteration of the Neld Mead method
for (i in 1:5) {
  contour(x, x, z, xlim = c(-5,5), ylim = c(-5,5))
  xx = res$simplex[[i]][1,]
  yy = res$simplex[[i]][2,]
  polygon(xx, yy)
}

#draw a gif picture to represent the Neld Mead method
library(animation)
ani.options = '~/Documents'
saveGIF(interval = 0.1,
        for (i in 1:15) {
          contour(x, x, z, xlim = c(-5,5), ylim = c(-5,5))
          xx = res$simplex[[i]][1,]
          yy = res$simplex[[i]][2,]
          polygon(xx, yy)
        }, movie.name = "Neld-mead method.gif")
saveGIF(
        for (i in 1:10) {
          filled.contour(x, x, z, color.palette = terrain.colors)
          xx = res$simplex[[i]][1,]
          yy = res$simplex[[i]][2,]
          polygon(xx, yy)
        },
        interval = 0.1, movie.name = "Neal-mead.gif",nmax = 30)

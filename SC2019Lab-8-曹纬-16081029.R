f <- function(x)(x[1]^2+x[2]^2) 
x1 <- c(1,1,2)
x2 <- c(1,2,2)
fx <- function(x1,x2,e=1e-10){
  i=0
  n <- length(x1)
  x <- cbind(x1,x2) #combine x[1] and x [2] to make vector x
  xl <- x
  while(i>=0){
    fx <- apply(x,1,f) #calculate the value of x
    a <- which.min(fx) #find the index which fx is minmum
    b <- which.max(fx) #find the index which fx is maxmum
    x.max <- x[b,] #find x[i] which the value of fx is max
    x.min <- x[a,] #find x[i] which the value of fx is min
    f.xmax <- f(x.max)
    f.xmin <- f(x.min)
    x <- x[-b,] #drop x[i] which the value of fx is max
    x0 <- apply(x,2,mean) #calculate the mean of x
    xr <- 2*x0 -x.max #calculate the reflected point
    fx <- apply(x,1,f)
    c <- which.max(fx) #find xn
    xn <- x[c,]
    f.xn <- f(xn)
    f.xr <- f(xr)
    
    if(f.xr<f.xn && f.xr>=f.xmin){ #case1:f(x1)<=f(xr)<f(xn)
      x <- rbind(x,xr)
    }
    
    if(f.xr < f.xmin){ #case2:f(x1)>f(xr)
      xe <- 2*xr - x0 #calculate expansion point
      f.xe <- f(xe)
      if(f.xe < f.xr) #case2.1:f(xe)<f(xr)
        x <- rbind(x,xe)
      else #case2.2:f(xe)>f(xr)
        x <- rbind(x,xr)
    }
    
    if(f.xr >= f.xn){ #case3:f(xn)<f(xr)
      xc <- 0.5*x.max+0.5*x0 #calculate contraction point
      f.xc <- f(xc)
      if(f.xc >= f.xmax){ #case3.1:f(xn+1)<f(xc)
        xi <- 0.5*(x.max-xn)+x0 #shrink:smaller simplex
        x <- rbind(x,xi)
      }
      else #case3.2:f(xn+1)<f(xc)
        x <- rbind(x,xc)
    }
    i=i+1
    xl <- rbind(xl,x)
    xm <- matrix(xl,ncol = 2)
    xu <- x-x.max
    xt <- t(xu)
    xp <- rbind(xt,rep(1,n))
    vol <- 0.5*det(xp) #calculate the volume of the vectors
    fx <- apply(x,1,f)
    if(abs(vol)<e) #criterion to stop the loop
      break
  }

    return(list(i,x,fx,xn,xm))
}

e <- fx(x1,x2)


d <- e[[5]]
x.1 <- d[,1]
x.2 <- d[,2]

library(animation)
saveHTML({
  for (i in 1:151) points(x.1[i],x.2[i],pch=20,ylim = c(-2,2),xlim = c(-2,2))
})


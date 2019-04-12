#######
# L 7 #
#######

NelderMead <- function(func,xy1,xy2,xy3)
  {
  n <- 0
  fx <- c()
  x0 <- c()
  od <- c()
  xylist <- list()
  xylist[[1]] <- xy1
  xylist[[2]] <- xy2
  xylist[[3]] <- xy3
  tlong <- ((xylist[[1]][1]-xylist[[2]][1])^2+(xylist[[1]][2]-xylist[[2]][2])^2)^1/2 + ((xylist[[1]][1]-xylist[[3]][1])^2+(xylist[[1]][2]-xylist[[3]][2])^2)^1/2 + ((xylist[[2]][1]-xylist[[3]][1])^2+(xylist[[2]][2]-xylist[[3]][2])^2)^1/2
  while (tlong > 10^-10) {
    tlong <- ((xylist[[1]][1]-xylist[[2]][1])^2+(xylist[[1]][2]-xylist[[2]][2])^2)^1/2 + ((xylist[[1]][1]-xylist[[3]][1])^2+(xylist[[1]][2]-xylist[[3]][2])^2)^1/2 + ((xylist[[2]][1]-xylist[[3]][1])^2+(xylist[[2]][2]-xylist[[3]][2])^2)^1/2
    x1 <- xylist[[1]][1]
    x2 <- xylist[[1]][2]
    fx[1] <- eval(func)
    x1 <- xylist[[2]][1]
    x2 <- xylist[[2]][2]
    fx[2] <- eval(func)
    x1 <- xylist[[3]][1]
    x2 <- xylist[[3]][2]
    fx[3] <- eval(func)
    od <- order(fx)
    x0[1] <- (xylist[[1]][1]+xylist[[2]][1]+xylist[[3]][1])/3
    x0[2] <- (xylist[[1]][2]+xylist[[2]][2]+xylist[[3]][2])/3
    xr <- 2 * x0 - xylist[[od[3]]]
    x1 <- xr[1]
    x2 <- xr[2]
    fxr <- eval(func)
    if( fxr >= fx[od[1]] & fxr < fx[od[3]])
    {
      xylist[[od[3]]] <- xr
    }
    else if(fxr < fx[od[1]])
    {
      xe <- x0 + 2*(xr - x0)
      x1 <- xe[1]
      x2 <- xe[2]
      fxe <- eval(func)
      if(fxe < fxr)
      {
        xylist[[od[3]]] <- xe
      }
      else
      {
        xylist[[od[3]]] <- xr
      }
    }
    else
    {
      xc <- x0 + 0.5 * (xylist[[od[3]]] - x0)
      x1 <- xc[1]
      x2 <- xc[2]
      fxc <- eval(func)
      if(fxc < fx[od[3]])
      {
        xylist[[od[3]]] <- xc
      }
      else
      {
        xylist[[od[1]]] <- xylist[[od[2]]] + 0.5*(xylist[[od[1]]] - xylist[[od[2]]])
        xylist[[od[2]]] <- xylist[[od[2]]] + 0.5*(xylist[[od[2]]] - xylist[[od[2]]])
        xylist[[od[3]]] <- xylist[[od[2]]] + 0.5*(xylist[[od[3]]] - xylist[[od[2]]])
      }
      n <- n+1
    }
  }
  x0[1] <- (xylist[[1]][1]+xylist[[2]][1]+xylist[[3]][1])/3
  x0[2] <- (xylist[[1]][2]+xylist[[2]][2]+xylist[[3]][2])/3
  print(x0)
  print(n)
  print(xylist)
  }

xy1 <- c(1,1)
xy2 <- c(2,2)
xy3 <- c(3,3)
NelderMead(x1^2+x2^2,xy1,xy2,xy3)

debug(NelderMead)

#######
# L 8 #
#######

a <- c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0)
A <- matrix(a, byrow=T, nrow=5)
At <- t(m)

alpha <- eigen(At)
ala <- alpha$values

matx <- matrix(c(-1,ala[1],ala[1],ala[1],ala[1],
                 ala[2],-1,ala[2],ala[2],ala[2],
                 ala[3],ala[3],-1,ala[3],ala[3],
                 ala[4],ala[4],ala[4],-1,ala[4],
                 ala[5],ala[5],ala[5],ala[5],-1), byrow=T, nrow=5)

solve(matx)


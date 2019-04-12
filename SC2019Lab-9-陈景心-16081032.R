#nelder-mead
eva <- function(x) {
  eval(expression(x1^2+x2^2),list(x1=x[1],x2=x[2]))
}

neldermead <- function(X) {
  A<-matrix(c(X[1],X[2],1,X[3],X[4],1,X[5],X[6],1),3,3)
  S<-0.5*abs(det(A))
  while(S>1e-6){  
    x11 <- X[1]
    x12 <- X[2]
    x21 <- X[3]
    x22 <- X[4]
    x31 <- X[5]
    x32 <- X[6]
    f1 <- eva(c(x11, x12))
    f2 <- eva(c(x21, x22))
    f3 <- eva(c(x31, x32))
    fv <- c(f1, f2, f3)
    x01 <- (X[2 * order(fv)[1] - 1] + X[2 * order(fv)[2] - 1]) / 2
    x02 <- (X[2 * order(fv)[1]] + X[2 * order(fv)[2]]) / 2
    xr1 <- x01 + (x01 - X[2 * order(fv)[3] - 1])
    xr2 <- x02 + (x02 - X[2 * order(fv)[3]])
    fr <- eva(c(xr1, xr2))
    if (fr <= sort(fv)[2] & fr > min(fv)) {
      X[2 * order(fv)[3] - 1] <- xr1
      X[2 * order(fv)[3]] <- xr2
      fv[order(fv)[3]] <- fr
    }
    else if (fr < min(fv)) {
      xe1 <- x01 + 2 * (x01 - X[2 * order(fv)[3] - 1])
      xe2 <- x02 + 2 * (x02 - X[2 * order(fv)[3]])
      fe <- eva(c(xe1, xe2))
      if (fe < fr) {
        X[2 * order(fv)[3] - 1] <- xe1
        X[2 * order(fv)[3]] <- xe2
        fv[order(fv)[3]] <- fe
      }
      else if (fr <= fe) {
        X[2 * order(fv)[3] - 1] <- xr1
        X[2 * order(fv)[3]] <- xr2
        fv[order(fv)[3]] <- fr
      }
    }
    else if (fr >= sort(fv)[2]) {
      xc1 <- x01 - 0.5 * (x01 - X[2 * order(fv)[3] - 1])
      xc2 <- x02 - 0.5 * (x02 - X[2 * order(fv)[3]])
      fc <- eva(c(xc1, xc2))
      if (fc < max(fv)) {
        X[2 * order(fv)[3] - 1] <- xc1
        X[2 * order(fv)[3]] <- xc2
        fv[order(fv)[3]] <- fc
      }
      else{
        X[2 * order(fv)[3] - 1] <- (X[2 * order(fv)[3] - 1] + X[2 * order(fv)[1] - 1]) / 2
        X[2 * order(fv)[3]] <- (X[2 * order(fv)[3]] + X[2 * order(fv)[1]]) / 2
        X[2 * order(fv)[2] - 1] <- (X[2 * order(fv)[2] - 1] + X[2 * order(fv)[1] - 1]) / 2
        X[2 * order(fv)[2]] <- (X[2 * order(fv)[2]] + X[2 * order(fv)[1]]) / 2
        fv[order(fv)[3]] <- eva(c(X[2 * order(fv)[3] - 1], X[2 * order(fv)[3]]))
        fv[order(fv)[2]] <- eva(c(X[2 * order(fv)[2] - 1], X[2 * order(fv)[2]]))
      }
    }
  A<-matrix(c(X[1],X[2],1,X[3],X[4],1,X[5],X[6],1),3,3)
  S<-0.5*abs(det(A))
  }
  cat('x1=',X[2 * order(fv)[1] - 1],'x2=', X[2 * order(fv)[1]], 'fx=', min(fv))
}

neldermead(c(1,1,1,2,2,2))

#pageranks
AT<-matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5,5)
AT
r<-eigen(AT)$vectors[,1]
r<-r/sum(r)
r


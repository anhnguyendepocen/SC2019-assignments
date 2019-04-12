newton<-function(a,b,fun,e){
  x1<-a
  x2<-b
  t<-c(a,b)
  p<- x1^2 - x1 * x2 + x2^2+exp(x2)
  q=100000000
  whle(abs(q-p)>e){
    p<-q
    y1<-t
    dxy<-deriv(fun,c("x1","x2"),hessian = TRUE,func=TRUE)
    dxy(a,b)
      .expr6 <- exp(x2)
      .value <- x1^2 - x1 * x2 + x2^2 + .expr6
      .grad <- array(0, c(length(.value), 2L), list(NULL, c("x1", "x2")))
      .hessian <- array(0, c(length(.value), 2L, 2L), list(NULL, c("x1", "x2"), c("x1", "x2")))
      .grad[, "x1"] <- 2 * x1 - x2
      .hessian[, "x1", "x1"] <- 2
      .hessian[, "x1", "x2"] <- .hessian[, "x2", "x1"] <- -1
      .grad[, "x2"] <- 2 * x2 - x1 + .expr6
      .hessian[, "x2", "x2"] <- 2 + .expr6
      attr(.value, "gradient") <- .grad
      attr(.value, "hessian") <- .hessian
    hessian<-rbind(.hessian[,,"x1"],.hessian[,,"x2"])
    grad<-rbind(.grad[,"x1"],.grad[,"x2"])
    y1<-c(a,b)
    g<-solve(hessian)
    h<-grad
    t=y1-g%*%h
    x1<-t[1,1]
    x2<-t[1,2]
    q<- x1^2 - x1 * x2 + x2^2+exp(x2)
  }
  print(p)
}

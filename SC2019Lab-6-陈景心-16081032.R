newton<-function(f,a0,b0,n=100,tol=0.000001){
  X<-as.matrix(c(a0,b0))
  df<-deriv(f,c("x1","x2"),hessian = TRUE,func=T)
  for(i in 1:n){
    b<-matrix(attributes(df(X[1],X[2]))$gradient,2,1)
    A<-matrix(attributes(df(X[1],X[2]))$hessian,2,2)
    X<-X-solve(A,b)
    if(solve(A,b)[1]<tol&solve(A,b)[2]<tol){
      cat("x1=",X[1],"x2=",X[2],"y=",eval(f,list(x1=X[1],x2=X[2])),"n=",i)
      break
    }
    else
      i=i+1
  }
}
newton(expression(x1^2-x1*x2+x2^2+exp(x2)),0,0)


#Write a function to find min & max
##(four parameters:func→function(expression);varname→independent variables;x,y→initial value;n→max interation times;e→error.)
Newtonraphson <- function(fun,varname,x,y,n,e){ 
  i=1
  while(i<n){
    ans <- c(x,y)
    dvar <- deriv(fun,varname,hessian = T,func=T)                           ##function to caculate gradient & hessian matrix
    dx <- dvar(x,y)                                                         ##caculate gradient & hessian matrix
    d <- attributes(dx)
    gradient.m <- matrix(d$gradient,nrow = 1)
    hessian.m <- matrix(d$hessian,nrow = 2)
    ans <- ans - gradient.m%*%solve(hessian.m)                              ##caculate next(x,y)
    x <- ans[1]
    y <- ans[2]
    i=i+1
    if(gradient.m[1]<e & gradient.m[2]<e)                                   ##criterion to stop loop
      break
  }
  return(ans)
}

##estimate initial value of function by drawing a plot
x<-y<- seq(from=-5,to=5)
fun <- function(x,y)(x^2-x*y+y^2+exp(y))
z<-outer(x,y,fun)
persp(x,y,z,theta=90, phi=0,expand=0.7,col="lightblue",axes = T,ticktype = "detailed")

##calculate max or min of [x^2-x*y+y^2+exp(y)]
fun <- expression(x^2-x*y+y^2+exp(y))                                                        
Newtonraphson(fun,c("x","y"),-10,10,10000,10^-10)

  




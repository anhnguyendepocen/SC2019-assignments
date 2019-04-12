###############
#Lab Session ?#
###############
#This is the main function using Newton_raphson which can get multiple extreme point
Opt_Newton_raphson<-function(fun,epsilon,limit)
{
  n <- length(limit)/2
  xlist<-c()
  for(i in 1:n)
  {xlist <- rbind(xlist,runif(1000,limit[2*n-1],limit[2*n]))}#random in limit to get the all of roots
  solvelist <-c()
  fxlist <- c()
  for (i in 1:1000){x_=xlist[,i]
  while(TRUE){
    xname <-c()
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))#get variable x1-xn
      xname<-c(xname,paste("x",j,sep = ""))}
    x=x_
    fx<-eval(fun)
    dx<-eval(deriv(fun,xname,hessian = TRUE))
    x_=x-solve(matrix(attributes(dx)$hessian,nrow = 2),t(attributes(dx)$gradient))
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))}
    x=x_
    fx_=eval(fun)
    flag<-0
    for(j in 1:n){
    if(x[j]<limit[2*j-1]|x[j]>limit[2*j]){flag<-1}}#judge whether over the limit
    if(flag){break}
    if((abs(fx_-fx)<epsilon))
    {solvelist <- rbind(solvelist,round(t(x_),7))
    fxlist <- rbind(fxlist,round(fx_,7))
    break}
  }}
  colnames(solvelist)=xname
  colnames(fxlist)=c("fx")#names them
  result<-cbind(solvelist,fxlist)
  unique_result<-unique.matrix(result)
  unique_result
}

fun<-expression(cos(x1)*cos(x2))#multiple extreme points
limit<-c(-4,4,-4,4)
fun<-expression(x1^2-x1*x2+x2^2+exp(x2))#Question
limit<-c(-1,1,-1,1)
result <- Opt_Newton_raphson(fun,1e-10,limit)
result
#debug
debug(Opt_Newton_raphson)
undebug(Opt_Newton_raphson)

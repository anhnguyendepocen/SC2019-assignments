###############
#Lab Session ?#
###############
#This is the main function using Newton_raphson which can get multiple extreme point
Opt_Newton_raphson<-function(fun,num_variable,time,epsilon,limit = NULL,bound = FALSE)
{
  n <- num_variable
  xlist<-c()
  if(is.null(limit)){limit <- rep(0,2*n)}
  for(i in 1:n)
  {xlist <- rbind(xlist,runif(time,limit[2*n-1],limit[2*n]))}#random in limit to get the all of roots
  solvelist <-c()
  fxlist <- c()
  for (i in 1:time){x_=xlist[,i]
  while(TRUE){
    xname <-c()
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))#get variable x1-xn
      xname<-c(xname,paste("x",j,sep = ""))}
    x=x_
    fx<-eval(fun)
    dx<-eval(deriv(fun,xname,hessian = TRUE))
    x_=x-solve(matrix(attributes(dx)$hessian,nrow = n),t(attributes(dx)$gradient))
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))}
    x=x_
    fx_=eval(fun)
    if(bound){
    flag<-0
    for(j in 1:n){
      if(x[j]<limit[2*j-1]|x[j]>limit[2*j]){flag<-1}}#judge whether over the limit
    if(flag){break}}
    if((abs(fx_-fx)<epsilon))
    {solvelist <- rbind(solvelist,round(t(x_),7))
    fxlist <- rbind(fxlist,round(fx_,7))
    break}
  }}
  if(length(fxlist)>1){
  colnames(solvelist)=xname
  colnames(fxlist)=c("fx")#names them
  result<-cbind(-solvelist,-fxlist)
  unique_result<-unique.matrix(result)
  unique_result}
  else{-solvelist}
}

#logistic function
logistic_fun<-function(y,x,time= 10, epsilon = 1e-10,limit = NULL){
  num_variable <- ncol(x)+1
  num_sample <-length(y)
  y<-y
  
  cfun <-'0'
  for(i in 1:num_sample){
    eta <- 'x1'
    for(j in 2:num_variable){
      eta <- paste(eta,"+x",j,"*",x[i,j-1],sep = "")
    }
    cfun <- paste(cfun,"+",y[i],"*(",eta,")-log(1+exp(",eta,"))",sep = "")
  }
  cfun
  fun <- parse(text = cfun)
  Opt_Newton_raphson(fun,num_variable,time,epsilon,limit)
  }
library(lattice)
library(DAAG)
anesthetic
y<-anesthetic$move
x<-anesthetic[2]
logistic_fun(y,x)
#glm() function
glm(formula = nomove ~ conc, family = binomial(link = "logit"), 
    data = anesthetic)
#function optim()
func = function(beta){
  beta1<-beta[1]
  beta2<-beta[2]
  y<-anesthetic$move
  x<-anesthetic$conc
  result <- 0
  for(i in 1:length(y)){
  result <- result + y[i]*(beta1+beta2*x[i])-log(1+exp(beta1+beta2*x[i]))}
  -result
}

optim(c(0,0),func)

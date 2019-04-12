###############
#Lab Session 5#
###############
#to remove abs,first we should judge which two bracket match
db <- function(left,right,match_b){
  if(length(left)!=0){
  for(i in length(left[[1]]):1){
    for(j in 1:length(right[[1]])){
      if(right[[1]][j]>left[[1]][i]){
        nleft<-left[[1]][-i]
        nright <- right[[1]][-j]
        match_b<-c(match_b,left[[1]][i],right[[1]][j])
        match_b<-db(nleft,nright,match_b)
        break
      }
    }
    break
  }}
  match_b
}
#then we can remove abs() with condition in abs(***)
remove_abs<- function(cfun)
{
  abs <- gregexpr("abs(.*?)",cfun)
  left <- gregexpr("\\(",cfun)
  right <- gregexpr("\\)",cfun)
  index_abs <- abs[[1]][1]
  match_b<-c()
  match_b<-db(left,right,match_b)
  index_left <- index_abs+3
  right_place <- which(match_b == index_left)
  index_right <- match_b[right_place+1]
  return_cfun1<-substr(cfun,1,index_abs-1)
  return_cfun2<-substr(cfun,index_left+1,index_right-1)
  return_cfun3<-substr(cfun,index_right+1,nchar(cfun))
  ex1<-paste(return_cfun1,return_cfun2,return_cfun3,sep = "")
  ex2<-paste(return_cfun1,"-(",return_cfun2,")",return_cfun3,sep = "")
  re<-list(ex1=ex1,ex2=ex2,condition=return_cfun2)
  re
}
#for abs() we should classified discussion,then use Newton_raphson
classify <- function(re,epsilon,a,b){
  xlist <- runif(10,a,b)
  solvelist <-c()
  if(length(grep("abs",re$ex1))!=0){re1<-remove_abs(ex1)
    classify(re1,epsilon,a,b)}
  if(length(grep("abs",re$ex2))!=0){re2<-remove_abs(ex2)
    classify(re2,epsilon,a,b)}
  else{
  for (i in xlist){x_=i
  while(TRUE){
    x=x_
    if(eval(parse(text = re$condition))>=0){
      ex1<-parse(text = re$ex1)
      fx<-eval(ex1)
      x_=x-eval(ex1)*eval(D(ex1,"x"))/((eval(D(ex1,"x")))^2-eval(ex1)*eval(D(D(ex1,"x"),"x")))
      x=x_
      fx_=eval(ex1)
      if(x<a|x>b){break}
      if((abs(fx_)<epsilon))
      {solvelist <- c(solvelist,round(x_,7))
      break}
    }
    else{
      ex2<-parse(text = re$ex2)
      fx<-eval(ex2)
      x_=x-eval(ex2)*eval(D(ex2,"x"))/((eval(D(ex2,"x")))^2-eval(ex2)*eval(D(D(ex2,"x"),"x")))
      x=x_
      fx_=eval(ex2)
      if(x<a|x>b){break}
      if((abs(fx_)<epsilon))
      {solvelist <- c(solvelist,round(x_,7))
      break}
    }
  }
  
  }
  }
  unique(solvelist)
}
#This is the main function of Newton_raphson which can deal with multiple root
Newton_raphson<-function(fun,epsilon,a,b)
{
  cfun <- as.character(fun)
  if(length(gregexpr("abs|sqrt",cfun))!=0){
  while((gregexpr("abs|sqrt",cfun)[[1]][1]==1)&substr(cfun,nchar(cfun),nchar(cfun))==")"){
    if(gregexpr("abs",cfun)[[1]][1]==1){
      cfun <- substr(cfun,4,nchar(cfun))
      cfun <- paste(cfun,"^2",sep = "")
    }
    else{
      cfun <- substr(cfun,6,nchar(cfun)-1)
    }
    if(length(gregexpr("abs|sqrt",cfun))==0){
      break}
  }}
  if(length(grep("abs",cfun))!=0){
    re <- remove_abs(cfun)
    classify(re,epsilon,a,b)
  }
  else{
    
  fun <- parse(text = cfun)
  
  xlist <- runif(10,a,b)#random in [a,b] to get the all of roots
  solvelist <-c()
  for (i in xlist){x_=i
  while(TRUE){
    x=x_
    fx<-eval(fun)
    #x_=x-eval(fun)/eval(D(fun,"x"))#x_=x-eval(fun)*eval(D(fun,"x"))/((eval(D(fun,"x")))^2-eval(fun)*D(D(fun,"x"),"x"))
    x_=x-eval(fun)*eval(D(fun,"x"))/((eval(D(fun,"x")))^2-eval(fun)*eval(D(D(fun,"x"),"x")))
    x=x_
    fx_=eval(fun)
    if(x<a|x>b){break}
    if((abs(fx_)<epsilon))
    {solvelist <- c(solvelist,round(x_,7))
      break}
  }}
  unique(solvelist)
  }
}
#Question
fun<-expression(x^2-5)
fun<-expression(sqrt(abs(x)))
fun<-expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2)
fun<-expression(abs(x)-1)#By improving the function we also can solve the function abs()
#plot approximate graph to estimate where are roots
x<-seq(-100,100,0.1)
y<-eval(fun)
plot(x,y)
lines(x,y)
#Newton_raphson to estimate the roots
Newton_raphson(fun,1e-10,-2,2)
## lab 7

beta <-c(700,-0.3)
data(orings, package="faraway")
orings<-orings[-1,]
x<-orings[,1]*0.01
y<-orings[,2]
# plot(x, y, col = "blue", pch = 20)

## Make the log normal likelihood function
func = function(x,y,beta){#似然函数
  sum(y*(beta[1]+beta[2]*x)-log(1+exp(beta[1]+beta[2]*x)))
}

grad = function(x,y,beta){#似然函数的一阶导
  matrix(c(sum(y-1/(1+exp(-beta[1]-beta[2]*x))),sum(y*x-x/(1+exp(-beta[1]-beta[2]*x)))),2,1)
}

hess = function(x,y,beta){#似然函数的二阶导
  matrix(c(sum(-1*exp(-beta[1]-beta[2]*x)/(1+exp(-beta[1]-beta[2]*x))^2),-1*sum(x*exp(-beta[1]-beta[2]*x)/(1+exp(-beta[1]-beta[2]*x))^2),-1*sum(x*exp(-beta[1]-beta[2]*x)/(1+exp(-beta[1]-beta[2]*x))^2),-1*sum(x*x*exp(-beta[1]-beta[2]*x)/(1+exp(-beta[1]-beta[2]*x))^2)),2,2)
}
logfun<-list(func,grad,hess)
NR.op2 <- function(fun,x,y,Epsilon = 10^-10,maxiter = 1000){#牛顿法求解
  result<-c()
  fun[1]->func
  fun[2]->grad
  fun[3]->hess
  for(num in 1:1000)
  {
    n<-0
    fx_<-1
    fx<-0#使循环参数返回初始值
    beta<-runif(2,-5,5)
    while ((abs(fx_-fx)>Epsilon) && n <= maxiter)#跳出循环的条件
    {
      n<-n+1
      fx_<-func(x,y,beta)#迭代前的函数值
      
      beta <- beta- solve(hess(x,y,beta),grad(x,y,beta))#对β值进行迭代

      fx<-func(x,y,beta)#迭代后的函数值
    }
    if (n > maxiter) {
      beta<-NULL
    } 
    if (is.null(x)==FALSE)
    {result <-c(result,round(beta,5))}#记录成功跳出循环的自变量
  }
  index<-duplicated(result)
  result[!index]#取消重复值
}
NR.op2(logfun,x,y)
debug(NR.op2)

#lab 8

optim(c(beta),x=x,y=y,func)#使用optim函数优化

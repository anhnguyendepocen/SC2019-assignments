#######
# L 6 #
#######

#写出最大似然函数
LHfunc <- function(x,y,b)
{
  sum(y*(b[1]+b[2]*x)-log(1+exp(b[1]+b[2]*x)))
}

#写出渐变矩阵函数
Gradfunc <- function(x,y,b)
{
  matrix(c(sum(y-1/(1+exp(-b[1]-b[2]*x))),sum(y*x-x/(1+exp(-b[1]-b[2]*x)))),2,1)
}

#写出Hessian矩阵函数
Hessfunc <- function(x,y,b)
{
  matrix(c(sum(exp(-b[1]-b[2]*x)/(1+exp(-b[1]-b[2]*x))^2),sum(x*exp(-b[1]-b[2]*x)/(1+exp(-b[1]-b[2]*x))^2),sum(x*exp(-b[1]-b[2]*x)/(1+exp(-b[1]-b[2]*x))^2),sum(x*x*exp(-b[1]-b[2]*x)/(1+exp(-b[1]-b[2]*x))^2)),2,2)
}

#3.
NRmethod4 <- function(Lfunc,Gfunc,Hfunc,x,y,min = 10^-10,maxtime = 1000)
{
  allx <- c()
  for(num in 1:1000)
  {
    n <- 0
    fx1 <- 0
    fx2 <- 1
    b <- runif(2,-5,5)
    while ((abs(fx2 - fx1)> min ) && n <= maxtime)
    {
      n <- n+1
      fx2 <- Lfunc(x,y,b)
      
      b <- b- solve(Hfunc(x,y,b),Gfunc(x,y,b))
      
      fx1 <- Lfunc(x,y,b)
    }
    if (n > maxtime) {
      b < -NULL
    } 
    if (is.null(x)==FALSE)
    {allx <-c(allx,round(b,5))}
  }
  index <- duplicated(allx)
  allx[!index]
}

NRmethod4(LHfunc,Gradfunc,Hessfunc,x,y)
debug(NRmethod4)
  
#第二个作业
optim(c(b),x=x,y=y,LHfunc)
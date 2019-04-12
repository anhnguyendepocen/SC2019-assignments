NR.op <- function(f ,a,b,Epsilon = 10^-10,maxiter = 1000){#单自变量最优化
  result <-c()
  df <- deriv(f, 'x', func = TRUE,hessian = T)
  for(num in 1:1000)
  {x <-runif(1,a,b)#随机生成初始值
  n <-0
  while (abs(as.numeric(attr(df(x), 'gradient')[1,])) > Epsilon && n <= maxiter)
  {
  n<-n+1
  dfx<-df(x)
  gradf <- as.numeric(attr(dfx, 'gradient')[1,])
  hessf <- attr(dfx, 'hessian')[1,,]
    x <- x- gradf/hessf
  }
  if (as.numeric(attr(df(x), 'gradient')[1,]) > Epsilon) {
    x<-NULL
  } 
  if (is.null(x)==FALSE)
  {result <-c(result,round(x,5))}
  }
  index<-duplicated(result)
  result[!index]#取消重复值
}

NR.op2 <- function(f,x,Epsilon = 10^-10,maxiter = 1000){#多自变量最优化求解，x为函数所有自变量的初始值
  xnum<-length(x)#得到自变量个数
  xs<-c()
  result<-c()
  xs<-c()
  for (i in 1:xnum){
    assign(paste("x",i,sep = ""),x[i])#对x1-xn进行赋值，得到x的第[i]的元素
    xs<-c(xs,paste('x',i,sep = ''))#得到字符串x1-xn作为deriv函数的参数
  }
  for(num in 1:1000)
  {
    n<-0
    fx_<-1
    fx<-0#使循环参数返回初始值
  while ((abs(fx_-fx)>Epsilon) && n <= maxiter)#跳出循环的条件
  {
    n<-n+1
    fx_<-eval(f)#迭代前的函数值
    dx<-eval(deriv(f,xs,hessian = TRUE))#求导
    x <- x- solve(matrix(attributes(dx)$hessian,nrow = 2),t(attributes(dx)$gradient))#对x值进行迭代
    for (i in 1:xnum){
      assign(paste("x",i,sep = ""),x[i])#更改x1-xn，得到迭代后的x的元素值
    }
    fx<-eval(f)#迭代后的函数值
  }
  if (n > maxiter) {
    x<-NULL
  } 
  if (is.null(x)==FALSE)
  {result <-c(result,round(x,5))}#记录成功跳出循环的自变量
  }
  index<-duplicated(result)
  result[!index]#取消重复值
}
h<-expression(x1^2-x1*x2+x2^2+exp(x2))
NR.op2(h,c(3,1))
debug(NR.op2)

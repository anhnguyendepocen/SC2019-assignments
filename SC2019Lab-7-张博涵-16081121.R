x <- rnorm(1000, 3, 2)
y <- 3*x + 1 + rnorm(1000)


library(Deriv)

# 生成对数似然函数表达式
mkexpr <- function(y, x){
  expr1 <- ''
  for(i in 1:length(y)){
    expr1 <- paste0(expr1,'+(',y[i],'-beta0-beta1*',x[i],')^2')
  }
  expr1 <- paste0('-(',expr1,')/2-',length(y),'*log(2*pi)/2')
  return(parse(text = expr1))
}

source("../Lab5/SC2019Lab-6-张博涵-16081121.R")


result <- MultiD.Newton.Raphson(mkexpr(y, x), namevec = c('beta0','beta1'), c(3,1))
# plot to show the parameter estimation
plot(x,y, col = "blue", main = 'y~x')
abline(a = result$x[1], b = result$x[2], col = 'red')

# deriv takes too much time
Rprof()
MultiD.Newton.Raphson(mkexpr(y, x), namevec = c('beta0','beta1'), c(3,1))
Rprof(NULL)
summaryRprof()



## logistics 回归
## 对数似然函数
func <- function(beta){
  eta <- exp(beta[1]+beta[2]*x)
  return(sum(y*(beta[1]+beta[2]*x)-log(1+eta)))
}
## 一阶偏导与海塞矩阵
func_deri <- function(beta){
  eta <- exp(beta[1]+beta[2]*x)
  gradient <- c(sum(y-eta/(1+eta)),sum(y*x-x*eta/(1+eta)))
  hessian <- matrix(
    c(-sum((eta*(1+eta)-eta*eta)/(1+eta)^2),
    -sum((eta*x*(1+eta)-eta*x*eta)/(1+eta)^2),
    -sum((eta*x*(1+eta)-eta*x*eta)/(1+eta)^2),
    -sum((x*eta*x*(1+eta)-eta*x*x*eta)/(1+eta)^2)), ncol = 2,nrow = 2)
  return(list(gradient = gradient,hessian = hessian))
}

# 结果
result <- MultiD.Newton.Raphson(func, c('beta0','beta1'), c(-6,4), dx = func_deri)

model.glm <- glm(nomove ~ conc,data = anesthetic, family = "binomial")
summary(model.glm)


## use optiom

func_gradient <- function(beta){
  eta <- exp(beta[1]+beta[2]*x)
  gradient <- c(sum(y-eta/(1+eta)),sum(y*x-x*eta/(1+eta)))
  return(gradient)
}

optim(c(-6,5), func, gr = func_gradient, method = "BFGS", control = list(fnscale = -1))

optim(c(-6,5), func, gr = func_gradient, method = "L-BFGS-B", control = list(fnscale = -1))



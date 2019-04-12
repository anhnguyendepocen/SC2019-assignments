likeli <- function(beta, x, y){
  #logistics回归的对数似然函数，x和y是样本数据，beta是参数
  sum(y*(beta[1]+beta[2]*x)-log(exp(beta[1]+beta[2]*x)+1))
}
grad <- function(beta, x, y){
  matrix(c(sum(y-1+1/(exp(beta[1]+beta[2]*x)+1)),
         sum(x*(y-1)+x/(exp(beta[1]+beta[2]*x)+1))),2,1)
}
hess <- function(beta, x, y){
  matrix(c(sum(-exp(beta[1]+beta[2]*x)/(exp(beta[1]+beta[2]*x)+1)^2),
           sum(-x*exp(beta[1]+beta[2]*x)/(exp(beta[1]+beta[2]*x)+1)^2),
           sum(-x*exp(beta[1]+beta[2]*x)/(exp(beta[1]+beta[2]*x)+1)^2),
           sum(-x*x*exp(beta[1]+beta[2]*x)/(exp(beta[1]+beta[2]*x)+1)^2)),2,2,byrow = T)
}
newton <- function(fun, grad, hess, beta0, x, y){
  #beta0是迭代初始值，x和y是样本数据
  n = 0
  repeat{
    beta = beta0
    beta0 = beta0 - solve(hess(beta0,x,y)) %*% grad(beta0,x,y)
    n = n+1
    if(abs(fun(beta0,x,y)-fun(beta,x,y)) < 10^(-10) | n >= 100) break
    #以前后两次似然函数值之差的绝对值小于10^(-10)或迭代次数大于一百为终止条件
  }
  return(beta0)
}

#生成数据
beta = c(1,1)
x = rnorm(1000,3,1)
eta = beta[1] + beta[2]*x
p = exp(eta)/(exp(eta)+1)
y = rbinom(1000, 1, p)
#opt_beta使用牛顿法得到的参数值
opt_beta = newton(likeli, grad, hess, c(0.9,1.1), x, y);opt_beta 
vlm <- glm(y ~ x, family = binomial)
summary(vlm)


#比较牛顿法和glm得到的eta值
opt_eta = opt_beta[1] + opt_beta[2]*x  
lm_eta <- vlm$coefficients[1] + vlm$coefficients[2]*x 
plot(x, opt_eta, type = 'l', col = 'red')
points(x, lm_eta, type = 'p')
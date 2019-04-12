Newton_Raphson <- function(fun, x, var){
  #fun是要优化的函数，x是起始值，var是变量名
  a <- deriv(fun, var, func = T,hessian = T)
  n = 0
  repeat{
    len_x = length(x)
    df = as.vector(attributes(a(x[1],x[2]))[[1]])
    ddf = matrix(attributes(a(x[1],x[2]))[[2]],len_x,len_x, byrow = T)
    #求出x处函数对应的一阶偏导和二阶偏导的值
    x0 = x
    x = x - solve(ddf)%*%df
    if(sum(abs(x - x0)) < 10^(-10) | n >= 100) break
    #以前后两次迭代自变量变化量绝对值之和小于10^(-10)为终止条件
    else n = n + 1
  }
  list(x = x0, '导数值' = df, '迭代次数' = n)
}
# debug(Newton_Raphson)
Newton_Raphson(expression(x1^2 - x1*x2 + x2^2 + exp(x2)), c(1,1), c('x1','x2'))

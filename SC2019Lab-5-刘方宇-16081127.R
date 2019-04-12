Newton_Raphson <- function(x0, f, e){
  #输入两次f(x)的表达式，一个作为函数求函数值，另一个用来求导数值
  df = D(e, 'x')
  n = 0
  while(abs(f(x0)) > 10^(-10) & n < 100){
    x = x0
    df_x0 = eval(df) #求f(x)在x0处的导数值
    x0 = x0 - f(x0)/df_x0
    n = n + 1
  }
  l = list(x = x0, 'f(x)' = f(x0), '迭代次数' = n)
  return(l)
}
# debug(Newton_Raphson)
Newton_Raphson(2, function(x){x^2 - 5}, expression(x^2 - 5))
Newton_Raphson(0.25, function(x){x^2^(1/4)}, expression((x^2^(1/4))))
Newton_Raphson(0.5, function(x){x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2}, expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2))
Newton_Raphson(0.6, function(x){x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2}, expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2))

x <-c(0,0)
x1 <- x[1]
x2 <- x[2]
##初始化x的值，并指出x的取值

f <- function(x){
  fun <- expression(y = x1 ^ 2 - x1 * x2 + x2 ^ 2 + exp(x2))  
  return(fun)
}
##输入要计算的函数

Newton_Raphson3 <- function(x){
  dt <- eval(f(x))  #初始化dt值
  x0 <- x   ##记录初始值
  i <- 0  ##记录迭代次数
  x1 <- x[1]   ##根据输入x得到x1的值
  x2 <- x[2]   ##根据输入x得到x2的值
  
  fx12 = expression(x1 ^ 2 - x1 * x2 + x2 ^ 2 + exp(x2))
  dxy = deriv(fx12, c("x1", "x2"), func = TRUE)
  dt1 <- eval(dxy(x1,x2))
  ##计算方程的一阶导数矩阵
  
  dxy2 <- deriv3(fx12,c("x1", "x2"), func=function(x1,x2){}, hessian = TRUE)
  dt2 <- eval(dxy2(x1,x2))
  ##计算方程的二阶导数矩阵
  sdt2 <- solve(dt2)
  ##对获得二阶导数矩阵进行逆矩阵处理
  
  while(abs(dt) > 1 * 10 ^ -10){
    i  <- i + 1
    x <- x - eval(dt1) * eval(sdt2) 
    print(x)  #输出每次迭代后结果
    dt <- eval(f(x))
    if(i > 100){break}   ##迭代次数超过20次结束循环
  }
  if(i > 100000){print("牛顿-拉夫森法失效")}
  ##进行循环，每次迭代，迭代次数加一，
  ##如果最终迭代次数超过了100000次，输出方法失效（防止无限循环）
  
  else{
    print(paste("方程解为：", x, ";初始值为：", x0, ";迭代次数为：", i))
  }
  ##最终输出我们的结果
}

Newton_Raphson3(c(5,499))
##运行函数，获取结果

x1 = -96
x2 = 398
x1 ^ 2 - x1 * x2 + x2 ^ 2 + exp(x2)
##根据我们的结果来判断我们的函数是否有效

##附：在研究函数中，我们可以看出来，函数有无数组解，因此我们每次的输出结果都是其中的一个解
##因为是二元函数，一个方程，所以解个数无限。欲要产生唯一解，应该是两个方程或以上！

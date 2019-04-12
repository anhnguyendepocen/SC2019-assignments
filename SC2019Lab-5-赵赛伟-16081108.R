####SC2019Lab-5-赵赛伟-16081108##
##1.Write R code to find the root of x2=5##
##方程输入函数
f <- function(x){
  fun <- expression(x ^ 2 - 5)
  return(fun)
}

##Newton-Raphson方法函数
Newton_Raphson <- function(x){
  dt <- eval(f(x))  #初始化dt值
  x0 <- x   ##记录初始值
  i <- 0 ##记录迭代次数
  while(abs(dt) > 1 * 10 ^ -10){
    i  <- i + 1
    x <- x - eval(f(x)) / eval(D(f(x),"x"))
    print(x)  #输出每次迭代后结果
    dt <- eval(f(x))
    if(i > 20){break}   ##迭代次数超过20次结束循环
  }
  if(i > 20){print("牛顿-拉夫森法失效")}
  else{
    print(paste("方程解为：", x, ";初始值为：", x0, ";迭代次数为：", i))
  }
}

Newton_Raphson(2)

##2.Now use your Newton-Raphson code to find the root of f(x)=√|x|. Try initial value 0.25##
##重新定义方程
f <- function(x){
  fun <- expression((x ^2) ^ 0.25)  ##因为绝对值不可导，所以如此变换
  return(fun)
}

##选取0.25为初始值，会无限循环迭代
Newton_Raphson(0.25)

##3.Now use your Newton-Raphson code to find the root of xe???x2=0.4(ex+1)???1+0.2. Try initial values 0.5 and 0.6##
##重新定义方程
f <- function(x){
  fun <- expression(x * exp(-1 * (x ^2)) - (0.4 * (exp(x) + 1) ^ (-1)) - 0.2)
  return(fun)
}

Newton_Raphson(0.5)
Newton_Raphson(0.6)

##从出现的错误中可以看出，初始值的设置是需要注意的；牛顿—拉夫森法是不完善的
##需要考虑是否有零点以及零点的数量
##我们绘制（2）（3）中的函数图像，即可看出：
curve(expr = (x ^2) ^ 0.25, xlim = c(-1,1))
curve(expr = x * exp(-1 * (x ^2)) - (0.4 * (exp(x) + 1) ^ (-1)) - 0.2, xlim = c(-10,10))
##（2）只有x=0的零点（两边皆大于零，不可迭代逼近）；（3）有两个零点（迭代会出现逼近更接近值，于是不同初始值结果不一样）


##根据后续课程优化函数
Newton_Raphson2 <- function(x){
  dt <- eval(D(f(x),"x"))  #初始化dt值
  x0 <- x   ##记录初始值
  i <- 0 ##记录迭代次数
  while(abs(dt) > 1 * 10 ^ -10){
    i  <- i + 1
    x <- x - eval(D(f(x),"x")) / eval(D(D(f(x),"x"),"x"))
    print(x)  #输出每次迭代后结果
    dt <- eval(D(f(x),"x"))
    if(i > 20){break}   ##迭代次数超过20次结束循环
    }
  if(i > 20){print("牛顿-拉夫森法失效")}
  else{
    print(paste("方程解为：", x, ";初始值为：", x0, ";迭代次数为：", i))
  }
}

##3.函数测试
f <- function(x){
  fun <- expression(x * exp(-1 * (x ^2)) - (0.4 * (exp(x) + 1) ^ (-1)) - 0.2)
  return(fun)
}
Newton_Raphson2(0.6)
Newton_Raphson2(0.1)

##2.函数测试
f <- function(x){
fun <- expression((x ^2) ^ 0.25)  ##因为绝对值不可导，所以如此变换
return(fun)
}
Newton_Raphson2(0.6)
Newton_Raphson2(0.1)
##我们可以看到，进行优化后，对于（2）类似的情况仍然没有很好的解决办法

####附：每次都要重新输入方程函数太过麻烦，考虑用户交互式输入函数####
fun <- expression()
fun <- edit(fun)
fun
##通过上述代码即可用户输入函数，将其变为一个变量
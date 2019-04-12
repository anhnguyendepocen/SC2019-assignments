#######
# L 6 #
#######

#1.
NRMethod1 <- function(x,min = 10^-10){
  yx = expression(x^2-5)
  y <- x^2 - 5
  while(abs(y) >= min)
  {
    x <- x - y/eval(D(yx,"x"))
    y <- x^2 - 5
  }
  print(x)
}

NRMethod1(1)

#2.
NRMethod2 <- function(x,min = 10^-10){
  yx1 = expression(x^1/2)
  yx2 = expression((-x)^1/2)
  y <- (abs(x))^1/2
  while(abs(y) >= min)
  {
    if(x>0)
    {    
      x <- x - y/eval(D(yx1,"x"))
      y <- (abs(x))^1/2
    }
    else
    {
      x <- x - y/eval(D(yx2,"x"))
      y <- (abs(x))^1/2
    }

  }
  print(x)
}

NRMethod2(0.25)
#由于不能对绝对值求导，所以需要在函数中加入正负值判断函数

#3.
NRMethod3 <- function(x,min = 10^-10){
  yx = expression(x*exp(-x^2)-0.4*(exp(x)+1)^-1+0.2)
  y <- x*exp(-x^2)-0.4*(exp(x)+1)^-1+0.2
  while(abs(y) >= min)
  {
    x <- x - y/eval(D(yx,"x"))
    y <- x*exp(-x^2)-0.4*(exp(x)+1)^-1+0.2
  }
  print(x)
}

NRMethod3(0.5)
NRMethod3(0.6)
undebug(NRMethod3)

#若使用0.6求导会导致x的值过小以致于在y达到需要精度前，x被认为是0
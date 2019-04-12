#lab5 
#find the root of x^2 = 4

f <- function(x){
  x^2-4
}
df <- function(x){
  2*x
}

nr_fun <- function(f,df, x0, tlevel = 10^(-5)){
  xn = NULL
  n = 0
  x = x0
  
  while(abs(f(x)) >= tlevel){
    n = n+1
    xn = x - f(x)/df(x)
    x = xn
  }
  x
}

nr_fun(f,df,3,0.001)#the result = 2.00001

#find the root of f(x) = |x|^(0.5),initial value = 0.25
#we could find that f(x)>=0 and equal to 0 when x = 0,
#and when x=0,the derivation of fx is not existed
f1 <- function(x){
  abs(x)^(0.5)
}
df1 <- function(x){
  while(x!=0){
    if(x>0) 
      return(1/2*x^(0.5))
    else 
      return(-1/2*(-x)^0.5)
  }
}

nr_fun(f1,df1,0.25)

debug(nr_fun)
#the x is always between 0.25 and -0.25 during the iteration,so we could not stop the loop

#find the root of x*exp(-x^2) = 0.4/(exp(x)+1)+0.2
f2 <- function(x){
  0.4/(exp(x)+1)+0.2-x*exp(-x^2)
}
a <- expression(0.4/(exp(x)+1)+0.2-x*exp(-x^2))
D(a,'x')
df2 <- function(x){
  -(0.4 * exp(x)/(exp(x) + 1)^2 + (exp(-x^2) - x * (exp(-x^2) * (2 * x))))
}

#initial value is 0.5
nr_fun(f2,df2,0.5,10^(-9))
#initial value is 0.6
nr_fun(f2,df2,0.6,10^(-9))
#the result is the same,0.4303877

#we could find more from the plot of the function f2
x <- -100:100
y <- f2(x)
plot(y,x)
#for this function,it doesn't have a root 
#and when x is approaching inf,the f2 is limited to 0.2(the min value of f2)

#from question 2 and 3,the Newton methon can't deal with some special initial value
#and it just get the minimum value when the function doesn't have a root of fx=0







#Coding Nelder Mead(第八节)
#minimum 
#(1,1),(1,2),(2,2)as the starting simplex
α=1
γ=2
ρ=0.5
f<-function(x){
  return(x[1]^2+x[2]^2)
}#用来求函数值
x1<-c(1,1)
x2<-c(1,2)
x3<-c(2,2)

fminimum<-function(α=1,γ=2,ρ=0.5,x1=c(1,1),x2=c(1,2),x3=c(2,2),n=10000){
for(i in 1:n){
x<-c(x1,x2,x3)
fx1<-f(x1)#求函数值
fx2<-f(x2)
fx3<-f(x3)
fx<-c(fx1,fx2,fx3)
order_fx<-sort(fx)#对函数值按小到大排序

for(i in 1:3){   #求x0,先找出可加的两点
  if(fx[i]==order_fx[1]){
    a=c(x[2*i-1],x[2*i])
  }
}   
for(i in 1:3){   #求x0,先找出可加的两点,a,b就是除去最差的那个点后剩下的点
  if(fx[i]==order_fx[2]){
    b=c(x[2*i-1],x[2*i])
  }
}
for(i in 1:3){   #找出最差点是哪个
  if(fx[i]==order_fx[3]){
    c=c(x[2*i-1],x[2*i])
  }
} 
x0<-(a+b)/2
x1=a#重新赋值，ab是保留的点,a是此时最优点，b是次优点,c是最差点
x2=b
x3=c
xr<-x0+α*(x0-x3)
fxr<-f(xr)

#case1
if(f(xr)<f(x3)&f(xr)>=f(x1)){
  x3=xr
}
#case2
if(fxr<fx1){
  xe<-x0+γ*(xr-x0)
  if(f(xe)<f(xr)){
    x3=xe
  }
  else{
    x3=xr
  }
}
#case3
if(f(xr)>=order_fx[2]){
  xc<-x0+ρ*(x3-x0)
  if(f(xc)<f(x3)){
    x3=xc
  }
}}
return(c(x1,f(x1)))}

fminimum(α=1,γ=2,ρ=0.5,x1,x2,x3,n=10000)
#[1] 1.083236e-162 2.983943e-163  0.000000e+00



#第九节作业
A=matrix(c(0,1,0,1,1,
           0,0,1,1,1,
           1,0,0,1,0,
           0,0,0,0,1,
           0,0,1,0,0),5,5,byrow = TRUE)
At<-t(A)
Aeigen<-eigen(At)
r<-Aeigen[1]
r
#$values
#[1]  1.659302+0.0000000i -0.471019+1.2276729i -0.471019-1.2276729i -0.358632+0.4689741i -0.358632-0.4689741i
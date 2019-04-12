#88888888#
α<-1
γ<-2
ρ<-0.5
#求解函数#
f <- function(x){
  return(x[1]^2+x[2]^2)
}
#初始值#
x1<-c(1,1)
x2<-c(1,2)
x3<-c(2,2)

DCXFmin <- function(α=1,γ=2,ρ=0.5,x1=c(1,1),x2=c(1,2),x3=c(2,2),n=3){
  for(i in 1:n){
    x <- c(x1,x2,x3)
    y <- c(f(x1),f(x2),f(x3))
    
    #对y排序（小->大）#
    re_y <- sort(y)
    #对x排序（小->大）#
    temp <- c()
    
    for(i in 1:3){
      for(j in 1:3){
        
        if(y[j] == re_y[i]){
          temp[2*i-1] <- x[2*j-1]
          temp[2*i] <- x[2*j]
        }
      }
    }
    #a：最小，c：最大#
    a <- c(temp[1:2])
    b <- c(temp[3:4])
    c <- c(temp[5:6])
    #较优两解的均值#
    x0 <- (a+b)/2
    #排完序的坐标重新赋值，用xn替换x3#
    x1 <- a
    x2 <- b
    x3 <- c
    xn <- x0+α*(x0-x3)
    
    #case1#
    if(f(xn) < f(x3)&f(xn) >= f(x1)){
      x3 <- xn
    }
    #case2#
    if(f(xn) < f(x1)){
      xe <- x0+γ*(xn-x0)
      if(f(xe) < f(xn)){
        x3 <- xe
      }
      else{
        x3 <- xn
      }
    }
    #case3#
    if(f(xn) >= re_y[2]){
      xa <- x0+ρ*(x3-x0)
      if(f(xa) < f(x3)){
        x3 <- xa
      }
    }}
  
  return(c(x1,f(x1)))}

DCXFmin(α=1,γ=2,ρ=0.5,x1,x2,x3,n=3)
DCXFmin(α=1,γ=2,ρ=0.5,x1,x2,x3,n=10000)



#999999999#
m <- matrix(c(0,1,0,1,1,
           0,0,1,1,1,
           1,0,0,1,0,
           0,0,0,0,1,
           0,0,1,0,0),5,5,byrow = TRUE)
tm <- t(m)
me <- eigen(tm)
r<-me[1]
r
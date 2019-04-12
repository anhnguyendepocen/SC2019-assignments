#Coding Nelder_Mead
Nelder_Mead <- function(f, x1, x2, x3){
  #输入参数为要优化的函数以及起始的单纯形
  x.list = list(x1, x2, x3)
  n = 0
  repeat{
    fx1 = do.call(f, as.list(x.list[[1]]))
    fx2 = do.call(f, as.list(x.list[[2]]))
    fx3 = do.call(f, as.list(x.list[[3]]))
    #找出单纯形中的最大最小值以及对应的自变量值
    min_fx = min(c(fx1,fx2,fx3))
    max_fx = max(c(fx1,fx2,fx3))
    min_index = which.min(c(fx1,fx2,fx3))
    max_index = which.max(c(fx1,fx2,fx3))
    x1 = x.list[[min_index]]
    xn = x.list[[max_index]]
    #判断是否停止迭代
    a = c(1,2,3)
    X = matrix(c(x1 - xn, x.list[[a[-c(min_index,max_index)]]]-xn),2,2)
    if(abs(det(X))/2 <= 10^(-10) | n > 100) {
      x.list[4] = n
      return(x.list)
      break
    }
    x0 = (x1 + x.list[[a[-c(min_index,max_index)]]])/2
    xr = 2*x0 - xn
    fxr = do.call(f, as.list(xr))
    #f(x1) <= f(xr) < f(xn)
    if(fxr >= min_fx  & fxr < max_fx) x.list[[max_index]] = xr
    #f(xr) < f(x1)
    else if(fxr < min_fx){
      xe = 2*xr - x0
      fxe = do.call(f, as.list(xe))
      if(fxe < fxr) x.list[[max_index]] = xe
      else x.list[[max_index]] = xr
    }
    #f(xr) >= f(xn)
    else {
      xc = (x0 + xn)/2
      fxc = do.call(f, as.list(xc))
      if(fxc >= max_fx){
        for(i in 1:3) x.list[[i]] = (x1 + x.list[[i]])/2
      }
      else x.list[[max_index]] = xc
    }
    n = n+1
  }
}
Nelder_Mead(function(x1,x2){x1^2+x2^2},c(1,1),c(1,2),c(2,2))



#计算特征向量r
A = matrix(c(0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0),5,5,byrow = T)
eig = eigen(t(A))$vectors
for(i in 1:5){
  a = eig[,i]
  eig[,i] = a/sum(a)
}
sum(eig[,1]);eig[,1]
sum(eig[,2]);eig[,2]
sum(eig[,3]);eig[,3]  #用循环无输出
sum(eig[,4]);eig[,4]
sum(eig[,5]);eig[,5]

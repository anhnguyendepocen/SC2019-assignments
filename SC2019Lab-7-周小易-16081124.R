#lab-7

data("mtcars")
y<-mtcars$vs
x<-mtcars$mpg	
#似然函数
L<-function(beta){
  sum( y*(beta[1]+beta[2]*x) - log(1+exp(beta[1]+beta[2]*x)) )
}
#梯度
g<-function(beta){
  e<-exp(beta[1]+beta[2]*x)
  return(c(sum(y-e/(1+e))  ,
    sum(y*x-x*e/(1+e))))
}
#hessian矩阵
h<-function(beta){
  e<-exp(beta[1]+beta[2]*x)
  
  return(matrix(c(sum(e/(1+e)^2),sum(x*e/(1+e)^2),sum(x*e/(1+e)^2), sum(x^2*e/(1+e)^2))))
  
}
beta<-newton.raphson(L,x0=c(1,0.05))
beta

glm(y~x,family = binomial)

optim(c(1,0.05),L,gr =g ,method ="BFGS",control = list(fnscale = -1))









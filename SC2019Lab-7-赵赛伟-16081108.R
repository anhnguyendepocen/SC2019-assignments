####SC2019Lab-7-赵赛伟-16081108####
##logistic回归模型的系数极大似然函数的牛顿-拉夫森法估计##
##为方便处理，默认为二元logistics回归模型##

##数据输入及处理
data(wcgs, package = "faraway")
##获取变量
x1 <- wcgs[,3]
x2 <- wcgs[,8]
chd <- wcgs[,10]

y <- c()   ##初始化

for(i in 1:3154){
  if(chd[i] == "no"){y[i] = 0}
  else{y[i] = 1}
}  ##转为0,1变量

##logistics回归模型的对数极大似然函数##
func = function(beta){
  eta <- beta[1] + beta[2] * x1 + beta[3] * x2
  sum(y * eta - log(1 + exp(eta)))
}

##梯度函数
grad = function(beta){
  eta <- beta[1] + beta[2] * x1 + beta[3] * x2
  matrix(c(sum(y - exp(eta) / (1 + exp(eta))),sum(x1 * (y - exp(eta) / (1 + exp(eta)))),
           sum(x2 * (y - exp(eta) / (1 + exp(eta))))),3,1)
}

##hessian函数
hess = function(beta){
  eta <- beta[1] + beta[2] * x1 + beta[3] * x2
  matrix(c(sum(-1*exp(eta) / (1 + exp(eta)) ^ 2),
           sum(x1 * (-1*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum(x2 * (-1*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum((-1*x1*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum(x1 * (-1*x1*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum(x2 * (-1*x1*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum((-1*x2*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum(x1 * (-1*x2*exp(eta) / (1 + exp(eta)) ^ 2)),
           sum(x2 * (-1*x2*exp(eta) / (1 + exp(eta)) ^ 2))),3,3)
}

##迭代函数及结果（这里用了其他同学比较好的函数）
Newton <- function(beta,epsilon,limit)
{
  eta <- beta[1] + beta[2] * x1 + beta[3] * x2
  n <- length(limit)/2
  xlist<-c()
  for(i in 1:n)
  {xlist <- rbind(xlist,runif(1000,limit[2*n-1],limit[2*n]))}#random in limit to get the all of roots
  solvelist <-c()
  fxlist <- c()
  for (i in 1:1000){x_=xlist[,i]
  while(TRUE){
    xname <-c()
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))#get variable x1-xn
      xname<-c(xname,paste("x",j,sep = ""))}
    x=x_
    fx<-eval(func(beta))
    x_=x - solve(hess(beta),t(grad(beta)))   ####这里无数次报错，完全不知道错位支出
    for(j in 1:n)
    {assign(paste("x",j,sep = ""),replicate(1,x_[j]))}
    x=x_
    fx_=eval(func(beta))
    flag<-0
    for(j in 1:n){
      if(x[j]<limit[2*j-1]|x[j]>limit[2*j]){flag<-1}}#judge whether over the limit
    if(flag){break}
    if((abs(fx_-fx)<epsilon))
    {solvelist <- rbind(solvelist,round(t(x_),7))
    fxlist <- rbind(fxlist,round(fx_,7))
    break}
  }}
  colnames(solvelist)=xname
  colnames(fxlist)=c("fx")#names them
  result<-cbind(solvelist,fxlist)
  unique_result<-unique.matrix(result)
  unique_result
}

optimOut <- Newton(c(1,3,1),1e-10,c(-1,1,-1,1))
beta0Hat <- optimOut[1]
beta1Hat <- optimOut[2]
beta2Hat <- optimOut[3]
yHat <- beta0Hat + beta1Hat * x1 + beta2Hat * x2

##测试运行结果
lm <- glm(chd ~ height + cigs, family = binomial, wcgs)
summary(lm)

####SC2019Lab-8-赵赛伟-16081108####
###使用optim()对Logistic回归模型进行最大似然检验###
optim(par = c(1,2,3),fn = func,gr = grad,method = "BFGS",hessian = FALSE)


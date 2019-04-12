###SC2019Lab-7-Shuaidong Zhu-16081133

#Logistic Regression likelihood function
logreglikeli <- function(beta){
  sum((y*(beta[1]+beta[2]*x)-log(1+exp(beta[1]+beta[2]*x))))
}

#gradient function
gradlogreg <- function(beta){
  matrix(c(sum(y-exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))),
           sum(x*(y-exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))))))
         
}
#hessian
hessianlogreg <- function(beta){
  matrix(c(-sum((exp(beta[1]+beta[2]*x)*(1+exp(beta[1]+beta[2]*x))-exp(2*(beta[1]+beta[2]*x)))/(1+exp(beta[1]+beta[2]*x))^2),
           -sum((exp(beta[1]+beta[2]*x)*x*(1+exp(beta[1]+beta[2]*x))-x*exp(2*(beta[1]+beta[2]*x)))/(1+exp(beta[1]+beta[2]*x))^2),
           -sum((exp(beta[1]+beta[2]*x)*x*(1+exp(beta[1]+beta[2]*x))-x*exp(2*(beta[1]+beta[2]*x)))/(1+exp(beta[1]+beta[2]*x))^2),
           -sum((exp(beta[1]+beta[2]*x)*(x^2)*(1+exp(beta[1]+beta[2]*x))-(x^2)*exp(2*(beta[1]+beta[2]*x)))/(1+exp(beta[1]+beta[2]*x))^2)),
             2,2)
}

#example data
library(lattice)
library(DAAG)
head(anesthetic)
y <- anesthetic$nomove
x <- anesthetic$conc

#Logistic Regression
anes1 <- glm(nomove~conc,family=binomial(link='logit'),data=anesthetic)
summary(anes1)


#Use Newton’s method
newtonlog <- newton_multoptim(logreglikeli,c(-5,5))
newtonlog


#optim function
minlogreglikeli <- function(beta){
  -sum((y*(beta[1]+beta[2]*x)-log(1+exp(beta[1]+beta[2]*x))))
}
mingradlogreg <- function(beta){
  matrix(c(-sum(y-exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))),
           -sum(x*(y-exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))))))
}
BFGSlog <- optim(c(-5,5),minlogreglikeli,mingradlogreg,method = "BFGS",hessian = TRUE)
BFGSlog

#plot
#Newton's method
par(mfrow = c(1,1))
optimOut <- newtonlog$extrm.root
beta0Hat <- optimOut[1]
beta1Hat <- optimOut[2]
etaHat <- beta0Hat + beta1Hat * x
pHat <- exp(etaHat)/(1+exp(etaHat)) #Probability Estimate
plot(x, pHat, pch = 20,main = "不同方法估计参数拟合曲线对比图")
points(sort(x), pHat[order(x)], type = "l", col = "red", lwd = 2)#newton's method line
#Glm 
myLRCoef <- anes1$coefficients
etaHatGLM <- myLRCoef[1] + myLRCoef[2] * x
pHatGLM <- exp(etaHatGLM)/(1+exp(etaHatGLM))#Probability Estimate
points(sort(x), pHatGLM[order(x)], type = "l",
       col = "blue", lty="dashed", lwd = 2, pch = 20)
#BFGS
BFGScoef <- BFGSlog$par
etaHatBFGS <- BFGScoef[1] + BFGScoef[2] * x
pHatBFGS <- exp(etaHatBFGS)/(1+exp(etaHatBFGS))#Probability Estimate
points(sort(x), pHatBFGS[order(x)], type = "l",col = "green", lty = 3,lwd = 2)
#图例
legend("bottomright",legend = c("Newton","GLM","BFGS"),col = c("red","blue","green"),lwd = 2)


############automatic identify dataset and variable

#some problems   
  #x1 x2 x3 y cannot be used in deriv function ? 
Newton_method <- function(f, x0, arg = NULL, i=0.00001, N = 100, gradient = NULL, hessian = NULL){
  #f: expression  x0: row vector
  # x1 = swiss$Examination
  # x2 = swiss$Agriculture
  # x3 = swiss$Education
  # y = swiss$indicator
  # arg <- cbind(y,x1,x2,x3)

  
  k <- c(10, 20)        #k[1]:previous solution  k[2]:present solution
  num <- 1         #count times
  vector_x <- x0  #initial value
  ch <- as.character(f)
  vari <- regmatches(ch, gregexpr("[A-Z]", ch))[[1]]
  vari <- unique(vari)
  
  
  if(is.null(arg)){
    if(is.null(gradient)&is.null(hessian)){
      
      #d <- deriv(f, c("x", "y"), hessian = T)  #expression;   
      
      #regular expression, to find varis, as the argment of deriv() 
      #No limit on num of varis
      #vabiables must be CAPITAL LETTER
      
      d <- deriv(f, vari, hessian = T, function.arg = T) 
      
      
      #gradient <- expression(attributes(eval(d))$gradient)   #as function: eval(grandient), without given x
      #hessian <- expression(matrix(attributes(eval(d))$hessian, byrow = F, 2,2))
    }
    else if(!is.null(gradient)&!is.null(hessian))(next)  
    else(return("Input the complete derivative expression or none"))    
    
    while(abs(k[2] - k[1]) > i){
      if(num > N){print("Can't solve");return(x0)}
      de <- do.call(d, as.list(vector_x))
      hessian <- matrix(attributes(de)$hessian, byrow = F, sqrt(length(attributes(de)$hessian)))
      gradient <- t(attributes(de)$gradient)
      vector_x <- vector_x - solve(hessian, gradient)
      k[1] <- k[2]
      k[2] <- de[1]
      num <- num+1
    }
  }
  
  
  else{
    y <- arg[,1]
    for(i in 2:ncol(arg)){
      assign(paste("x",i-1,sep = ""), arg[,i])
    }
    d <- deriv(f, vari, hessian = T, function.arg = T)
    while(abs(k[2] - k[1]) > i){
      if(num > N){print("Can't solve");return(x0)}

      re <- do.call(d, as.list(vector_x))
      
      he <- attr(re,"hessian")



      hessian <- rbind(apply(he[,,1],2,sum),apply(he[,,2],2,sum),apply(he[,,3],2,sum),apply(he[,,4],2,sum))
      gradient <- as.matrix(apply(attr(re,"gradient"),2,sum))
      value <- sum(re)

      
      print(hessian)
      
      vector_x <- vector_x - solve(hessian, gradient)
      k[1] <- k[2]
      k[2] <- value
      num <- num+1
    }
  }
  
  eigenvalue <- eigen(hessian)$values
  if(all(eigenvalue>0)){
    extrem <- "Maximum Point"
  }
  else if(all(eigenvalue<0)){
    extrem <- "Minimun Point"
  }
  else{
    extrem <- "saddle point"
  }
  list(extrem, vector_x = vector_x, value = k[2])
}



# f = expression(X^2 -X*Y + Y^2 + exp(Y))
# Newton_method(f,c(0,1))
# 
# f = expression(X^2 -X*Y + Y^2 + exp(Y) + Z^2 + X*K^2)
# 
# Newton_method(f,c(1,1,1,1))



library(faraway)
swiss$indicator <- as.numeric(swiss$Fertility>80)

f = expression((y*(A + B*x1 + C*x2 + D*x3) - log(1+exp(A + B*x1 + C*x2 + D*x3))))
a = swiss$Examination
b = swiss$Agriculture
x= swiss$Education
y = swiss$indicator


swissdata <- cbind(y,a,b,x)

Newton_method(f,c(1,1,1,1),arg = swissdata)



############V2.0   
#specify dataset and x1,x2.... in advance
  
Newton_method <- function(func, x0, multi_obs = F, i=0.00001, N = 100, gradient = NULL, hessian = NULL){


  k <- c(10, 20)        #k[1]:previous solution  k[2]:present solution
  num <- 1         #count times
  vector_x <- x0  #initial value
  f <- func
  ch <- as.character(f)
  vari <- regmatches(ch, gregexpr("[A-Z]", ch))[[1]]
  vari <- unique(vari)
  
  
  if(!multi_obs){
    if(is.null(gradient)&is.null(hessian)){
      
      #d <- deriv(f, c("x", "y"), hessian = T)  #expression;   
      
      #regular expression, to find varis, as the argment of deriv() 
      #No limit on num of varis
      #vabiables must be CAPITAL LETTER
      
      d <- deriv(f, vari, hessian = T, function.arg = T) 
      
      
      #gradient <- expression(attributes(eval(d))$gradient)   #as function: eval(grandient), without given x
      #hessian <- expression(matrix(attributes(eval(d))$hessian, byrow = F, 2,2))
    }
    else if(!is.null(gradient)&!is.null(hessian))(next)  
    else(return("Input the complete derivative expression or none"))    
    
    while(abs(k[2] - k[1]) > i){
      if(num > N){print("Can't solve");return(x0)}
      
      de <- do.call(d, as.list(vector_x))
      hessian <- matrix(attributes(de)$hessian, byrow = F, sqrt(length(attributes(de)$hessian)))
      gradient <- t(attributes(de)$gradient)
      vector_x <- vector_x - solve(hessian, gradient)
      k[1] <- k[2]
      k[2] <- de[1]
      num <- num+1
    }
  }
  
  
  else{
    

    while(abs(k[2] - k[1]) > i){
      if(num > N){print("Can't solve");return(x0)}
      hessian <- matrix(0,4,4)
      gradient <- matrix(0,4)
      value <- 0
      for(i in 1:length(X1)){
        y <- Y[i]
        x1 <- X1[i]
        x2 <- X2[i]
        x3 <- X3[i]
        f <- func
        d <- deriv(f, vari, hessian = T, function.arg = T)
        re <- do.call(d, as.list(vector_x))
        
        he <- attr(re,"hessian")
        hessian <- hessian + rbind(he[,,1],he[,,2],he[,,3],he[,,4])
        gradient <- gradient + t(attr(re,"gradient"))
        value <- value + re[1]
      }
      
      
      vector_x <- vector_x - solve(hessian, gradient)
      k[1] <- k[2]
      k[2] <- value
      num <- num+1
    }
  }
  
  eigenvalue <- eigen(hessian)$values
  if(all(eigenvalue>0)){
    extrem <- "Maximum Point"
  }
  else if(all(eigenvalue<0)){
    extrem <- "Minimun Point"
  }
  else{
    extrem <- "saddle point"
  }
  list(extrem, vector_x = vector_x, value = k[2])
}





#####logsitic regression

#f = expression(sum(y*(A + B*x1 + C*x2 + D*x3) - log(1+exp(A + B*x1 + C*x2 + D*x3))))   #function (sum) is not in Differential table


library(faraway)
swiss$indicator <- as.numeric(swiss$Fertility>80)

f = expression((y*(A + B*x1 + C*x2 + D*x3) - log(1+exp(A + B*x1 + C*x2 + D*x3))))
X1 = swiss$Examination

X2 = swiss$Agriculture
X3 = swiss$Education
Y = swiss$indicator

Newton_method(f,c(4,-1,-1,-1),multi_obs = T)






######simulation
f <- function(Beta){
  A <- Beta[1]; B <- Beta[2]; C <- Beta[3]; D <- Beta[4]
  x1 = swiss$Examination
  x2 = swiss$Agriculture
  x3 = swiss$Education
  y = swiss$indicator
  (sum(y*(A + B*x1 + C*x2 + D*x3) - log(1+exp(A + B*x1 + C*x2 + D*x3))))
}


re<-optim(c(1,1,0,1), f, control = list(fnscale = -1))
re$par
fit$coefficients



fit <- glm(indicator ~ Examination + Agriculture +  Education, family=binomial, data = swiss)
summary(fit)

y*(A + B*x1 + C*x2 + D*x3) - log(1+exp(A + B*x1 + C*x2 + D*x3))


x1 = swiss$Examination[1]
x2 = swiss$Agriculture[1]
x3 = swiss$Education[1]
y = swiss$indicator[1]
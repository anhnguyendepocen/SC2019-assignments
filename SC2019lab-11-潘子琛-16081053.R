#Generalised inverse
  #Simple linear regression;
  x<-c(1,3,5,6,7,9,10,15,20,28)
  y<-c(4,24,30,56,78,90,105,140,195,245)
  plot(y~x)
  lm(y~x)  
  #9.184 
  beta <- ginv(x) %*% y  
  beta
  #9.25614

  #multiple linear regression 
  A <- matrix(c(11, 32, 43, 66, 28, 67, 89, 34, -55, -67, -80, -97), 4, 
            3)
  b <- 5 * A[, 1] - 9 * A[, 3] + 1e-04 * (rnorm(4) - 0.5) 
  library(MASS)
  beta <- ginv(A) %*% b  
  beta
  #5.000004e+00  -8.999995e+00
  lm(b ~ A[, 1]+A[, 3])  
  #5.0000052  -8.9999937
  
#The Application of QR decomposition
  #Simple linear regression;
  x<-c(1,3,5,6,7,9,10,15,20,28)
  y<-c(4,24,30,56,78,90,105,140,195,245)
  plot(y~x)
  lm(y~x)   
  #9.184
  qrresult<-qr(x)
  Qreslt <- qr.Q(qrresult);Qreslt  #Get Q
  Rreslt <- qr.R(qrresult);Rreslt  #Get R
  Xreslt <- qr.X(qrresult);Xreslt
  beta <-solve(t(Qreslt)%*%Qreslt%*%Rreslt,t(Qreslt)%*%y)
  beta
  #9.25614
  
  #multiple linear regression 
  A <- matrix(c(11, 32, 43, 66, 28, 67, 89, 34, -55, -67, -80, -97), 4, 
              3)
  b <- 5 * A[, 1] - 9 * A[, 3] + 1e-04 * (rnorm(4) - 0.5)
  qrresult1<-qr(A)
  Qreslt1 <- qr.Q(qrresult1);Qreslt1
  Rreslt1 <- qr.R(qrresult1);Rreslt1
  Xreslt1 <- qr.X(qrresult1);Xreslt1
  beta1 <-solve(t(Qreslt1)%*%Qreslt1%*%Rreslt1,t(Qreslt1)%*%b)
  beta1
  #5.000006e+00  -8.999996e+00
  lm(b ~ A[, 1]+A[, 3])  
  #5.0000052  -8.9999937
  
#The Application of svd
  #Simple linear regression;
  x<-c(1,3,5,6,7,9,10,15,20,28)
  y<-c(4,24,30,56,78,90,105,140,195,245)
  plot(y~x)
  lm(y~x)   
  #9.184 
  x.svd<-svd(x)
  x.svd_U <- x.svd$u;x.svd_U  #Get U
  x.svd_D <- x.svd$d;x.svd_D  #Get D
  x.svd_V <- x.svd$v;x.svd_V  #Get V
  beta <-solve(t(x.svd_U)%*%x.svd_U%*%x.svd_D%*%t(x.svd_V),t(x.svd_U)%*%y)
  beta
  #9.25614
  
  #multiple linear regression 
  A <- matrix(c(11, 32, 43, 66, 28, 67, 89, 34, -55, -67, -80, -97), 4, 
              3)
  b <- 5 * A[, 1] - 9 * A[, 3] + 1e-04 * (rnorm(4) - 0.5)
  A.svd<-svd(A)
  A.svd_U <- A.svd$u;A.svd_U  #Get U
  A.svd_D <- A.svd$d;A.svd_D  #Get D
  A.svd_V <- A.svd$v;A.svd_V  #Get V
  beta <-solve(t(A.svd_U)%*%A.svd_U%*%diag(A.svd_D)%*%t(A.svd_V),t(A.svd_U)%*%b)
  beta
  #4.999996e+00 -9.000003e+00
  lm(b ~ A[, 1]+A[, 3])  
  #5.0000052  -8.9999937
  
##   Conclusion: Through three different decomposition methods,
## we can see that although the means are different, 
## the final results are consistent with the linear fitting results.
 
  
  
  
  
#The Application of PowerMethod in PageRank 
  #transition probability matrix 
  probabilityMatrix<-function(G){
    cs <- colSums(G)
    cs[cs==0] <- 1
    n <- nrow(G)
    A <- matrix(0,nrow(G),ncol(G))
    for (i in 1:n) 
      A[i,] <- A[i,] + G[i,]/cs
    A
  }

  A<-matrix(c(0,0,1,0,0,
              1,0,0,0,0,
              0,1,0,0,1,
              1,1,1,0,0,
              1,1,0,1,0),5,5,byrow = T) 

  A<-probabilityMatrix(A);A


  #Realization of PowerMethod
  PowerMethod<-function(S,e,alpha,ep=1e-15) {
    A = alpha*S + ((1-alpha)/ncol(S))*e%*%t(e)
    P0<-matrix(c(0.2,0.1,0.4,0.2,0.1),5,1);k<-0    #initial value:P0
    repeat{       #iteration
      k<-k+1
      P1<-P0
      P0<-A%*%P0
      norm<-t(P0-P1)%*%(P0-P1)
      if(norm<ep){   #end condition
        break;      
      } 
    }
    print(P0)
  }
  debug(PowerMethod)
  C<-PowerMethod(A,matrix(c(1,1,1,1,1),5,1),0.85);C   


#QR decomposition 
 inputdata<-matrix(c(1,2,3,4),2,2,byrow=T)
 qrresult <- qr(inputdata)  

 Qreslt <- qr.Q(qrresult);Qreslt  #Get Q(Orthogonal matrix )
 Rreslt <- qr.R(qrresult);Rreslt  #Get R(triangular array)
 Xreslt <- qr.X(qrresult);Xreslt  


 
 
  
  
    
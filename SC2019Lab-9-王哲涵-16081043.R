A <- matrix(rep(1,18),nrow = 3)
T <- svd(A)
A2 <- T$u %*% diag(T$d) %*% t(T$v)
T
A2
diag(T$d)

student<- data.frame(  
    x1=c(148,139,160,149,159,142,153,150,151),  
    x2=c(41 ,34 , 49 ,36 ,45 ,31 ,43 ,43, 42),  
    x3=c(72 ,71 , 77 ,67 ,80 ,66 ,76 ,77,77),  
    x4=c(78 ,76 , 86 ,79 ,86 ,76 ,83 ,79 ,80)  
) 
student.pr <- princomp(student,cor=TRUE)
summary(student.pr,loadings=TRUE)
screeplot(student.pr,type="lines")
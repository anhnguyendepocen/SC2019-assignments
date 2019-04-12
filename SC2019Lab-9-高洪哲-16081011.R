setwd("D:/ProgramProject/R/Lab8")
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),nrow = 5)
AT <- t(A)
m <- eigen(AT)
alpha <- 1/m$values
alpha
r <- m$vectors
for(i in 1:5){
  assign(paste("r",i,sep = ""),r[i,])
}
mo <- Mod(r)
mod <- sum(mo)
R <- r/mod
sum(Mod(R))

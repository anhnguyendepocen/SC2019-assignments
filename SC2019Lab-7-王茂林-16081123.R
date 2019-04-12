################
#select problem#
################
#limit the value in iteratin and set more initial value, for this problem :/exp(x)
####################
#Coding Nelder Mead#
####################
library(animation)
fun <- function(x, y){x^2+y^2}

x <- seq(-4, 4, length = 50)
y <- x
f <- outer(x, y, fun(x,y))
plot(x,y)
x_initial<-c(1,1,2)
y_initial<-c(1,2,2)
x_all <- cbind(x_initial,y_initial)
Nelder_Mead <- function(x_all,alpha = 1.0,gama = 2.0,rou = 0.5,S = 0.5){
  listx_all <- lapply(seq_len(ncol(x_all)),function(i) x_all[,i])
  f_value <- do.call(fun,listx_all)
  which_worst <- which.max(f_value)
  x_worst <- x_all[which_worst,]
  which_best <- which.min(f_value)
  x_best <- x_all[which_best,]
  x_centroid <-apply(x_all[-which_worst,],2,mean)
  x_reflect <- x_centroid+alpha*(x_centroid-x_worst)
  f_reflect <-do.call(fun,lapply(seq_len(length(x_reflect)),function(i) x_reflect[i]))
  if(f_reflect<f_value[which_best]){
    x_expansion <- x_centroid + gama*(x_reflect-x_centroid)
    if(do.call(fun,lapply(seq_len(length(x_expansion)),function(i) x_expansion[i]))<do.call(fun,lapply(seq_len(length(x_reflect)),function(i) x_reflect[i]))){
      x_all<-x_all[-which_worst,]
      x_all <- rbind(x_all,x_expansion)
    }
    else{x_all<-x_all[-which_worst,]
    x_all <- rbind(x_all,x_reflect)
    }
  }
  else{
    if(do.call(fun,lapply(seq_len(length(x_reflect)),function(i) x_reflect[i]))>f_value[which_worst]){
      x_contracted <- x_centroid + rou*(x_worst-x_centroid)
      if(do.call(fun,lapply(seq_len(length(x_worst)),function(i) x_worst[i]))<= do.call(fun,lapply(seq_len(length(x_contracted)),function(i) x_contracted[i]))){
        x_all_list <- lapply(seq_len(nrow(x_all)),function(i) {x_all[which_best]+S*(x_all[i,]-x_all[which_best])})
        for (i in 1:length(x_all_list)){x_all[i,] <- x_all_list[[i]]
          }
      }
      else{
        x_all<-x_all[-which_worst,]
        x_all <- rbind(x_all,x_contracted)
      }
    }
    else{x_all<-x_all[-which_worst,]
      x_all <- rbind(x_all,x_reflect)
    }
  }
  x_all
}

x_all <- cbind(x_initial,y_initial)
saveGIF(
{
flag<-1
while(1/2*abs(det(d))>1e-5|flag ==1){ 
  x_all<-Nelder_Mead(x_all)
  x_all
  d <- as.matrix(x_all)
  for(i in 1:nrow(d)){
    d[i,]=d[i,]-d[nrow(d),]
  }
  d<-t(d[-nrow(d),])
  filled.contour(x,y,f,color = heat.colors, plot.axes = {axis(1);axis(2);polygon(x_all[,1], x_all[,2])})
  flag <- 0
  }}
)

######
#Rank#
######
A= c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0)
m <- matrix(A,nrow = 5)
x <- (eigen(t(m))$vectors)[, 1]

p <- x/sum(x)
p
#rank 5 4 3 1 2
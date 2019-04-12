####sc2019Lab-4-赵赛伟-16081108####
##为了使用debugging，我编一个解一元二次方程的简单小函数##
##函数
equation <- function(a,b,c){
  print(paste("x1 = ", ((-1) * b + (b * b - 4 * a * c)^0.5) / 2 * a))
  print(paste("x2 = ", ((-1) * b - (b * b - 4 * a * c)^0.5) / 2 * a))
}
equation(1,1,1)
#当a=b=c=1的时候没有实数解

equation2 <- function(a,b,c){
  if(b * b - 4 * a * c > 0){
    print(paste("x1 = ", ((-1) * b + (b * b - 4 * a * c)^0.5) / 2 * a))
    print(paste("x2 = ", ((-1) * b - (b * b - 4 * a * c)^0.5) / 2 * a))
  }
  else{print("没有实数解")}
}
equation2(1,1,1)
equation2(NA,NA,NA)
#当输入值无意义时，出现错误

equation4 <- function(a,b,c){
  if (is.na(a)|is.na(b)|is.na(c)){
    print("输入值没有意义")
  } 
  else if(b * b - 4 * a * c > 0){
    print(paste("x1 = ", ((-1) * b + (b * b - 4 * a * c)^0.5) / 2 * a))
    print(paste("x2 = ", ((-1) * b - (b * b - 4 * a * c)^0.5) / 2 * a))
  }
  else if(b * b - 4 * a * c == 0){
    print(paste("x = ",(-1) * b  / 2 * a))
  }
  else{print("没有实数解")}
}
equation4(NA,NA,NA)
equation4(NA,NA,1)
equation4(1,2,1)
equation4(1,3,1)
equation4(1,1,1)
#以上我们的函数就debug完了

##为了运行traceback函数，我们将之前的函数分开
dt <- function(a,b,c){
  dt = b * b - 4 * a * c
}

result <- function(a,b,c){
    x1 <- ((-1) * b + (b * b - 4 * a * c)^0.5) / 2 * a
    x1 <- ((-1) * b - (b * b - 4 * a * c)^0.5) / 2 * a
}

out <- function(a,b,c){
  if (is.na(a)|is.na(b)|is.na(c)){print("输入值没有意义")} 
  else if(dt(a,b,c) < 0){print("无实数解")}
  else{
    print("x1",x1)
    print("x2",x2)
  }
}

out(1,2,1)
traceback()

debug(out)
out(1,2,1)
##我们通过debugging可以看出问题出在输出函数out()上

##测试函数运行时间
library(doMC)
registerDoMC(cores = detectCores())
system.time({
  a = 1 
  b = 2 
  c = 1 
  if (is.na(a)|is.na(b)|is.na(c)){
    print("输入值没有意义")
  } 
  else if(b * b - 4 * a * c > 0){
    print(paste("x1 = ", ((-1) * b + (b * b - 4 * a * c)^0.5) / 2 * a))
    print(paste("x2 = ", ((-1) * b - (b * b - 4 * a * c)^0.5) / 2 * a))
  }
  else if(b * b - 4 * a * c == 0){
    print(paste("x = ",(-1) * b  / 2 * a))
  }
  else{print("没有实数解")}
})

system.time({
  n <- 1000
  r <- numeric(n)
  for (i in 1:n) {
    x <- rnorm(n)
    r[i] <- mean(x)
  }
})
##由此例可看出函数没问题，时间为0是因为运行时间太短


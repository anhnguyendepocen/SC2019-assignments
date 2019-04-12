#Lab_4_2 
## In this lab, write your own code, enjoy the tools of debugging and profiling and write a short report of optimizing your code.
## 1.DEBUG
# write a simple function
test<-function(x){
  y = x - 6
  if (y > 0){
    print("hello")
  }
}
test(10)#正常输出
test(-1)#无输出结果

debug(test)#开始debug
test(-1)

# solution
test2<-function(x){
  y = x - 6
  if (y > 0){
    print("hello")
  }
  else{
    print("bye")
  }
}
test2(-1)#正常输出

## 2.PROFILE
data(seeds, package = "faraway")
library(plyr)
library(doMC)
library(foreach)
library(iterators)
library(parallel)
registerDoMC(cores = detectCores())

#computes the time (in seconds) needed to execute an expression.
system.time({
  plot(germ /100 ~ moisture, seeds, xlim = c(1, 11), ylim = c(0, 1),
       xlab = "Moisture Level", ylab = "Germination Percentage")
  lmod <- lm(germ /100 ~ moisture, seeds)
})

#The R Profiler
Rprof()
plot(germ /100 ~ moisture, seeds, xlim = c(1, 11), ylim = c(0, 1),
     xlab = "Moisture Level", ylab = "Germination Percentage")
lmod <- lm(germ /100 ~ moisture, seeds)
Rprof(NULL)
#summaryRprof_ummarizes the output of Rprof() and gives percent of time spent in each function
summaryRprof(filename = "Rprof.out")

#Interactive Visualizations for Profiling R Code
library(profvis)
profvis({
  plot(germ /100 ~ moisture, seeds, xlim = c(1, 11), ylim = c(0, 1),
       xlab = "Moisture Level", ylab = "Germination Percentage")
  lmod <- lm(germ /100 ~ moisture, seeds)
})




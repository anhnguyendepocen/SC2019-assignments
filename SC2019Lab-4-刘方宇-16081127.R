f <- function(x){
  if(x > 0) f(x-1)
  invisible(cat(x))
}
debug(f)
f('a')
undebug(f)
system.time(f(1000))
#system.time(f(10000))输入参数为10000时，迭代算法占用空间过大，直接停止运行

#使用循环实现一个相同功能的函数，比较二者时间以及空间效率上的差异
g <- function(x){
  for(i in 0:x) cat(i)
}
system.time(g(1000))
system.time(g(10000))
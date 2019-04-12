library(dplyr)
#读入数据
pools1 <- read.table("E:\\swimming_pools.csv",sep = ",",header = T);pools1
pools2 <- read.csv("E:/swimming_pools.csv",sep=",");pools2
#输出数据
write.table(pools1, "E:/file1", sep = ",")
dput(pools1, "E:/file2")
save(pools2, file  = "E:/file3")
#重新读取保存数据
read.csv("E:/file1")
dget("E:/file2")
#删除pools2
rm(pools2)
#重新读入pools2
load("E:/file3");pools2
pools1[1]
pools1[c(1,2)];class(pools1[1])
pools1[[1]];class(pools1[[1]])

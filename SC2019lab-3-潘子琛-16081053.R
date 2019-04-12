#从https://yanfei.site/docs/dpsa/babanews.txt加载文本并在屏幕上打印。文本文件包含了阿里巴巴的一些新闻。             
babanews<-readLines("C:/Users/95898/Desktop/BABAnews.txt",encoding="UTF-8")
babanews

#这篇文章有几段？       
length(babanews)

#修剪每个段落的前导空格（尝试？？修剪）。      
library(dplyr)
for (i in 1:length(babanews))
  print(substr(babanews[i], which(strsplit(babanews[i], split=' ') %>% unlist(.) != ''), nchar(babanews[i])))

#文章中有多少个字符？       
nchar<-0
for (i in 1:length(babanews))
  nchar<-nchar+nchar(babanews[i])
print(nchar)

#将段落折叠成一段并显示在屏幕上（取消列出）。     
paste(babanews,collapse = ' ')
   
#文本是否包含单词“技术架构”？       
a<-grepl('技术架构',babanews)
a

#把文章分成句子（按句点）。     
strsplit(babanews,split='。')

#将“双11”替换为“双十一”。
gsub("双11", "双十一", babanews)




#计算前n项和的函数
s<-function(n){
  sum<-0
  for (i in 1:n)
    sum<-sum+i
  print(sum)
}
#为函数启动一个交互式调试器，一次一个表达式地单步执行R函数
debug(s)
s(100)

#启动分析器，运行求和函数
Rprof()
s<-function(n){
  sum<-0
  for (i in 1:n)
    sum<-sum+i
  print(sum)
}
s(100)
system.time(s(100))
s(200)
s(1000)
Rprof(NULL)

#求和函数花费时间的分析
library(profvis)
profvis({
  s<-function(n){
    sum<-0
    for (i in 1:n)
      sum<-sum+i
    print(sum)
  }
  s(100)
  system.time(s(100))
})




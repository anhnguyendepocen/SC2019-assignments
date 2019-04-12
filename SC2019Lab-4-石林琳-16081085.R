#(1)加载文本并打印
news<-readLines('C:/Users/Por/Desktop/R/BABAnews.txt')
Encoding(news)<-'UTF-8'
news

#(2)文章段落数
length(news)#段落数为5

#(3)删去每个段落前面的空格
news<-trimws(news,which = c("left"))
news

#(4)文章字符数
sum(nchar(news))#字符数为952

#(5)将段落合并成一个并列出
news<-paste(news,collapse=" ")
unlist(news)

#(6)文本是否包含单词'技术架构'？
grepl('技术架构',news)#包含

#(7)将‘双11’替换成‘双十一’
news<-gsub('双11|双 11','双十一',news)
news

#(8)将文章按句点分成句子
g<-gregexpr("(.*?)。",news)
news<-regmatches(news,g)
news



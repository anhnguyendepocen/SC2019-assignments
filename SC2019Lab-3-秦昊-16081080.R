#lab 3
#读取并打印文本信息
baba_news <- readLines("/Users/qinhao/R\ code/BABAnews.txt")
print(baba_news)

#文章有多少段
length(baba_news)

#trim leading whitespaces of each paragraph
#trimws()用于除去字符串开头和结尾的空格
news <- trimws(baba_news[1:5])

#文章中有多少个字符
#nchar()用于返回一个字符向量包含多少元素
nchar(news)

#将段落合为一个并打印出来
news_text <- paste0(news[1], news[2], news[3], news[4], news[5])
print(news_text)

#文本中包含“技术架构”这个词吗
grepl("技术架构", news_text)

#将文本按照时期划分成为句子，可以通过。和？划分句子
strsplit(news_text, "\\。|\\？")

#替换双11为双十一
#文中有两种“双11”，带空格和不带空格的
gsub("双\\s*11\\s*", "双十一", news_text)

#找到之前出错的代码，并debug
load.file <- function(filename){
  location <- paste("/Users/qinhao/Downloads/temp/",filename)
  x <- read.csv(location)
  x[,1] <- list(as.POSIXlt(x$time))
  x
}
#我之前在写load.file时，对paste函数不了解，默认会在两个字符串之间有一个空格
#会导致文件位置出错，“Error in file(file, "rt") : 无法打开链结”
file_name <- "Brisbane.csv"
load.file(file_name)

#用debug
debug(load.file)
load.file(file_name)
#一行一行运行可以看到在函数内部，location参数文件位置和文件名之间有空格，导致出错
#在查看了paste函数后，发现sep默认会有一个空格，所以将paste改为paste0






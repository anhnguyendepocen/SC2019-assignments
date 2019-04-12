## 1.Load text and print it on screen. Text file contains some of the news of Alibaba.
# 录入txt文件
BABA <- readLines("C:/Users/THINK/Desktop/大三下作业/统计计算/Lab-3/BABAnews.txt", encoding = "UTF-8")
# 读取文本文件
BABA

## 2.How many paragraphs are there in the article?
length(BABA)

## 3.Trim leading whitespaces of each paragraph (try ??trim).
# 根据help内容选择合适的函数
??trim
# 删除文件中的段前空格
BA <- trimws(BABA, which = "left"); BA

## 4.How many characters are there in the article?
# 将各段落字符长度存入列表
char_num <- list(nchar(BA));char_num
# 将列表的各项加和，计算文章的总字符数
lapply(char_num, sum)# 用lappy函数

## 5.Collapse paragraphs into one and display it on the screen (un-list it).
# 将五个段落合并，用空格分隔
baba <- paste(BA, collapse = ""); baba

## 6.Does the text contain word '技术架构'?
# 用grepl查找文本是否含有“技术架构”,并返回逻辑值
g <- grepl("技术架构", baba); g

## 7.Split the article into sentences (by periods).
ali <- strsplit(baba, "。"); ali

## 8.Replace '双11' with '双十一'.
alibb <- gsub(("双(.?)11"), "双十一", baba); alibb


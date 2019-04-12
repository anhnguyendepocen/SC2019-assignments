News <- readLines("E:/迅雷下载/BABAnews.txt", encoding = "UTF-8")

#How many paragraphs are there in the article?
length(News)

#Trim leading whitespaces of each paragraph
library(stringr)
News <- str_trim(News, side = 'left');News

#How many characters are there in the article?
sum(sapply(News, nchar))

#Collapse paragraphs into one and display it on the screen (un-list it).
News <- paste(News, collapse = "\n");News

#Does the text contain word '技术架构'?
gregexpr('技术架构', News)

#Split the article into sentences (by periods).
News <- strsplit(News, split = '。');News

#Replace '双11' with '双十一'.
gsub('双11','双十一', News)

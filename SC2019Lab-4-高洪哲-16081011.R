setwd("D:/ProgramProject/R/Lab4")
library(raster)
#1 Load text
BABAnews <- readLines("BABAnews.txt", encoding = "UTF-8")
BABAnews
mode(BABAnews)
#2.How many paragraphs are there in the article?
length(BABAnews)

#3.Trim leading whitespaces of each paragraph (try ??trim).
news <- trim(BABAnews)

#4.How many characters are there in the article?
sum(nchar(BABAnews))

#5.Collapse paragraphs into one and display it on the screen (un-list it).
y <- NULL
for (i in 1:length(news)){
   y <- paste(y,news[i])
}
news <- trim(y)
news

#6.Does the text contain word '技术架构'?
length(grep("技术架构",news))>0

#7.Split the article into sentences (by periods).
sentences <- strsplit(news,"。")
sentences

#8Replace '双11' with '双十一'. '双 11 '
news <- gsub("双11|双 11 ","双十一",news)
news

setwd("E:/好好学习 天天向上/各科复习资料/统计计算")

#load the text and print it 
BABAnews<-readLines("数据/BABAnews1.txt")
BABAnews

#the number of paragraphs
length(BABAnews)

#Remove leading  whitespace from character strings
BABA<-trimws(BABAnews,which = "left")

#the number of characters
sum(nchar(BABA))

#concatenate the paragraphs
BABA1<-paste(BABA[1],BABA[2],BABA[3],BABA[4],BABA[5])

#confirm if the text contains word "技术架构"
grepl("技术架构",BABA)

#Split the article into sentences (by periods)
strsplit(BABA1,split = "。")

#replace '双11' with '双十一'
gsub("双11|双 11","双十一",BABA)

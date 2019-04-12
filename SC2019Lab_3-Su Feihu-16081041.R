###SC2019   Lab Session 3
###name:Su Feihu    StudentID:16081041

setwd("D:/study/statistical computing")
##Load text from the https://yanfei.site/docs/dpsa/BABAnews.txt; 
##and print it on screen. Text file contains some of the news of Alibaba.
Alibabanews <- readLines("BABAnews.txt")
Encoding(Alibabanews) <- "UTF-8"
print(Alibabanews)

##How many paragraphs are there in the article?
length(Alibabanews)

##Trim leading whitespaces of each paragraph (try ??trim).
Alibabanews <- gsub(" ", "", Alibabanews)
Alibabanews

##How many characters are there in the article?
sum(nchar(Alibabanews))

##Collapse paragraphs into one and display it on the screen (un-list it).
Alibabanews <- Reduce('paste', Alibabanews) 
Alibabanews

##Does the text contain word '技术架构'?
Ali <- grepl("技术架构", Alibabanews)
Ali

##Split the article into sentences (by periods).
news <- gsub("。","。//", Alibabanews)
news <- gsub("！","！//", news)
news <- gsub("？","？//", news)
news

news <- strsplit(news, "//")
news

#Replace '双11' with '双十一'.
Alibabanews <- gsub("双11", "双十一", Alibabanews)
Alibabanews

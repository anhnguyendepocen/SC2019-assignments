######section 1#########
#Load text, encoding = ""UTF-F" to avoid mojibake
setwd("D:/statistical computing/Data")
news <- readLines("BABAnews.txt", encoding = "UTF-8")

#How many paragraphs
p <- length(news)

#Trim leading whitespaces of each paragraph 
??trim
news <- trimws(news, "left")

#How many characters
nchar(news)  #each paragraph
sum(nchar(news)) #total passage

#Collapse paragraphs into one and display it on the screen (un-list it)
news <- Reduce('paste', news) 

#Does the text contain word '技术架构'
g <- grep("数据构架", news)
length(g)

#Split the article into sentences (by periods).
strsplit(news, "。|！|？")[[1]]   #Error in last sentence
strsplit(news, "。[^。”] |！|？")[[1]]  # [^。”] : Not accept [。”] error!
strsplit(news, "。(?!”)|！|？")[[1]]  # [^。”] : Not accept [。”] error!

#Problem!

#keep punctuation?
sub("。","。}}", news)
news <- gsub("。","。}}", news)
news <- gsub("！","！}}", news)
news <- gsub("？","？}}", news)
news

news <- strsplit(news, "}}")[[1]]
news

#Replace '双11' with '双十一'.
news <- gsub("双(.?)11", "双十一", news)
news


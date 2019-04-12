baba<-readLines("D:/BUAA/2019春/统计计算/BABAnews.txt",encoding = "UTF-8")
baba
length(baba) #paragraphs
baba<-trimws(baba,"l") #trim leading whitespaces
nchar(baba) #characters
baba<-paste(baba[1],baba[2],baba[3],baba[4],baba[5]) #Collapse paragraphs into one 
baba
grepl("技术架构",baba)
g<-grep("技术架构",baba)
length(g) #contains 1 "技术架构"
strsplit(baba,"。") #Split the article into sentences
baba<-gsub("双.?11.?","双十一",baba) #Replace '双11' with '双十一'
baba

####SC2019Lab-3####
##1.读入数据并print
news <- readLines("C:/Users/Lenovo/Desktop/BABAnews.txt", encoding="UTF-8")

print(news)

##2.文章段数
length(news)

##3.trim每一段开头
trim.news <- gsub("^\\s+|\\s+$", "", news)
trim.news


##4.文章字数
nchar(news)
num.char1 = 0
for(i in 1:5){
  num.char1 = num.char1 + nchar(news[i])
}
num.char1

nchar(trim.news)
num.char2 = 0
for(i in 1:5){
  num.char2 = num.char2 + nchar(trim.news[i])
}
num.char2

##5.段落折叠为一段(这里我们用了去除开头和结尾空格的文本)
new = ""
for(i in 1:5){
  new  = paste(new, trim.news[i])
}
new

##6.查找“技术架构”
g <- grep("技术架构", new)
g
r <- regexpr("技术架构(.*)", news)
regmatches(news[1:5], r)
r
r2 <- regexpr("技术架构(.*?)", news)
regmatches(news[1:5], r2)
r2

##7.将文章按句号分为句子
strsplit(new, split = "。")

##8.将“双11”改为“双十一”
renew <- gsub("双11", "双十一", new)
renew

#我们发现还有的双11中有空格分隔
renew2 <- gsub("双11|双 11 ", "双十一", new)
renew2


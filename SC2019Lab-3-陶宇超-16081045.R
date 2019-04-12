#Statistical Computing Homework3
#author:Yuchao Tao  StudentID:16081045

#Read the txt document "ali" and print it on screen
ali <- readLines("/Users/tyc_219/Desktop/BABAnews.txt")
ali
#The document's paragraph 
length(ali)
#Trim leading whitespaces of each paragraph 
library(stringr)
ali <- str_trim(ali, 'left')
ali
#The document's characters
nchar(ali) %>% sum
#Collapse paragraphs into one and display it on the screen
ali <- Reduce('paste', ali)
ali
#Does the text contain word '技术架构'?
tec <- grepl("技术架构", ali)
#Split the article into sentences by periods
strsplit(ali, "。")
#Replace '双11' with '双十一'
ali <- gsub("双11", "双十一", ali)
ali <- gsub("双 11 ", "双十一", ali)
ali

#use brower, debug and trace to debugging my function
counts <- function(count)
{
  sum <- 0
  for (i in 1:count)
  {
    sum <- sum + i
    num <- sum/((i-1) * (i-2))
  }
  browser()
  print(sum)
  print(num)
}
results <- counts(100)

debug(counts)
counts(20)

trace(counts, sum)
counts(15)
untrace(counts)
trace(counts, browser())
counts(18)







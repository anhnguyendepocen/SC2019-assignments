######第三次作业######

#1.Load text 
dat<-readLines("C:/Users/Ozu/Desktop/BABAnews.txt",encoding = "UTF-8")
dat

#2.How many paragraphs are there in the article?   &    How many characters are there in the article?
length(dat)
sum(nchar(dat))

#3.Trim leading whitespaces of each paragraph.
for(i in 1:length(dat)){
  dat1[i]<-sub("    ","",dat[i])}
dat1
dat

#4.Collapse paragraphs into one and display it on the screen (un-list it).
for(i in 2:length(dat)){
  dat[i]<-sub("    ","",dat[i])}
dat3<-paste(dat[1],dat[2],dat[3],dat[4],dat[5])
dat3

#5.Does the text contain word '技术架构'?
g<-grep("技术架构",dat)
g

#6.Split the article into sentences (by periods).
strsplit(dat,split = "？|。")


#7.Replace '双11' with '双十一'.
for(i in 1:length(dat)){
  dat[i]<-gsub("双11","双十一",dat[i])}
dat






######第四次作业######

#In this lab, write your own code, enjoy the toosl of debugging 
#and profiling and write a short report of optimizing your code.

#the tools of debugging
f <- function(x) {
  r <- x - g(x)
  r
}
g <- function(y) {
  r <- y * h(y)
  r
}
h <- function(z) {
  r <- log(z)
  if (r < 10)
    r^2
  else r^3
}
f(1)
traceback()
debug(f)
f(-1)
#profiling 
system.time(f(-1))
#[1] 1
#exiting from: f(-1)
#用户 系统 流逝 
#0.07 0.01 1.72 
Rprof()
f(-1)
Rprof(NULL)

library(profvis)
profvis(f(-1))

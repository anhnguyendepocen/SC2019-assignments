## Dealing with text data

# Load text from .txt
setwd("D://BUAA//大三下//统计计算 康雁飞")
news <- readLines("BABAnews.txt", encoding = "UTF-8")
news

# The number of paragraphs in the article
length(news) # output: 5

# Trim whitespaces
library(stringr)
news2 <- str_trim(news, side = "left")

# Number of characters
sum(nchar(news2)) # output: 947

# Collapse paragraphs into one
news3 <- ""
for (i in 1:5) {
  news3 <- paste(news3, news2[i], sep = "")
}
news3

# Find the word "技术架构"
g1 <- grep("技术架构", news3)
length(g1) # Output = 1, indicating that the word does exist

# Split the article into sentences
news4 <- strsplit(news3, "。")
for (i in 1:length(news4[[1]])-1) {
  news4[[1]][i] <- paste(news4[[1]][i], "。", sep = "")
} # Add "。" back
news4[[1]]

# Replace "双11" with "双十一"
news5 <- gsub("双11","双十一",news3)
news5

## Debugging and profiling R code
# Original functions with bug
f1 <- function(x) {
  MeanLevel <- mean(f2(x))
  result <- f3(f2(x), MeanLevel) 
  return(result)
} # Calculate the mean value of a vector generated in f2
f2 <- function(y) {
  V <- rnorm(-20, -10, 1)
  return(V)
} # Generate y evenly distributed random numbers
f3 <- function(z,q) {
  zz <- z[20] - log(q)
  return(zz)
} # Calculate log and specific element of a vector

# Try running f1
f1(10) # An error outputted

# Try traceback
traceback() #It shows that something wrong occoured in rnorm

# Try debug
debug(f1)
f1(10) # The browser shutted down during the second line
undebug(f1)

# Try recover
options(error = recover)
f1(10)

# Try Rprof to profile
# This function is used to collect data from csv and try the correlation among different factors
factors2015 <- read.csv("C:/Users/zhuyu/Desktop/factors_2015.csv", header = TRUE, sep=",")
b2015 <- read.csv("C:/Users/zhuyu/Desktop/b_2015.csv", header = TRUE, sep=",")

b2015 <- b2015[,1]

VC03 <- factors2015[,"HC01_VC03"] #家庭总数
VC06 <- factors2015[,"HC01_VC06"] #有孩子的家庭数
VC13 <- factors2015[,"HC01_VC13"] #流浪汉数
VC14 <- factors2015[,"HC01_VC14"] #独居者数

VC28 <- factors2015[,"HC01_VC28"] #所有家庭中的孩子个数
VC31 <- factors2015[,"HC01_VC31"] #单亲家庭的孩子数

VC40 <- factors2015[,"HC01_VC40"] #离婚数

VC79 <- factors2015[,"HC01_VC79"] #高中入学人数
VC80 <- factors2015[,"HC01_VC80"] #高校入学人数

VC106 <- factors2015[,"HC01_VC106"] #未成年人数
VC204 <- factors2015[,"HC01_VC204"] #非洲裔人数

count <- 0
timess <- 0
# Use Rprof during for cycle
Rprof()
for (k in 1:13){
  fit2015gam <- gam(b2015 ~ s(as.numeric(unlist(factors[k]))))
  fit2015 <- lm(b2015 ~ as.numeric(unlist(factors[k])))
  print(summary(fit2015)$r.squared)
  print(summary(fit2015gam)$dev.expl)
  timess <- timess + 1
  if (summary(fit2015gam)$dev.expl > 0.1 | summary(fit2015)$r.squared > 0.5){count <- count + 1}
}
Rprof(NULL)

#It seems that printing every possible summaries makes the cycle lengthy
###SC2019Lab-3-朱帅东-16081133

#Part I: Load text and print it on screen
setwd("G:/大三下学期/统计计算")
Adata <- readLines("Lab-3/BABAnews.txt",encoding = "UTF-8")
Adata

#Part II: Number of paragraphs
length(Adata)
##[1] 5

#Part III: Trim leading whitespaces of each paragraph
Adata <- trimws(Adata,which = "left")
Adata

#Part IV: Number of characters in the article
sum(nchar(Adata))
##[1] 947

#Part V: Collapse paragraphs into one and display it on the screen
Adataf <- as.data.frame(Adata)
Adatap <- paste(Adataf[1:nrow(Adataf),1],collapse=" ")
Adatap

#Part VI:
gdata <- grepl("技术架构", Adata)
gdata  
##[1] FALSE  TRUE FALSE FALSE FALSE
##输出结果说明第二段含有技术架构，而其他段落则没有

#Part VII:Split the article into sentences
r <- gregexpr("(.*?)。", Adatap)
regmatches(Adatap, r)

#Part VIII:Replace '双11' with '双十一'
gsub("双11","双十一",Adata)

###SC2019Lab-4-朱帅东-16081133

##构造函数计算不同的置信区间构造法的包含对数正态分布数据真实峰度的覆盖率
Truecoveragerate <- function(n,B,num)
{
  NORM.num <- 0
  PIVOTAL.num <- 0
  Quantile.num <- 0
  true.skew <- (exp(1)+2)*sqrt(exp(1)-1)##对数正态分布的真实峰度
  
  for(j in 1:num)
  {
    datasampleY <- rnorm(n,0,1)
    datasampleX <- exp(datasampleY)
    data.skew <- skewness(datasampleX)
    
    #使用Bootstrap方法估计偏度
    Skew.data <- NULL
    SD.data <- NULL
    for (i in 1:B)
    {
      Xsample <- sample(datasampleX,n,T)
      Skew.per <- skewness(Xsample)
      Skew.data <- c(Skew.data,Skew.per)
    }
    FinalSD <- sd(Skew.data)
    ##置信度为0.95的置信区间
    #正态
    Lcl1 <- data.skew + qnorm(0.025,0,1)*FinalSD
    Ucl1 <- data.skew - qnorm(0.025,0,1)*FinalSD
    #NORM.interval <- c(Lcl1,Ucl1)
    if (true.skew >= Lcl1 & true.skew <= Ucl1)
    {
      NORM.num <-  NORM.num + 1
    }
    #枢轴量
    Lcl2 <- 2*data.skew - quantile(Skew.data,0.975)
    Ucl2 <- 2*data.skew - quantile(Skew.data,0.025)
    #PIVOTAL.interval <- c(Lcl2,Ucl2)
    if (true.skew >= Lcl2 & true.skew <= Ucl2)
    {
      PIVOTAL.num <-  PIVOTAL.num + 1
    }
    #分位数
    Lcl3 <- quantile(Skew.data,0.025)
    Ucl3 <- quantile(Skew.data,0.975) 
    #Quantile.interval <- c(Lcl3,Ucl3)
    if (true.skew >= Lcl3 & true.skew <= Ucl3)
    {
      Quantile.num <-  Quantile.num + 1
    }
  }
  list(NORM.rate = NORM.num/num,
       PIVOTAL.rate = PIVOTAL.num/num,
       Quantile.rate= Quantile.num/num)
}

#固定重抽样次数，改变样本大小
system.time({
N.RATEd <- NULL
P.RATEd <- NULL
Q.RATEd <- NULL
N <- c(50,100,200)
for(i in 1:3)
{
  n <- N[i]   
  turecover2 <- Truecoveragerate(n,1000,1000)
  N.RATE <- turecover2$NORM.rate
  P.RATE <- turecover2$PIVOTAL.rate
  Q.RATE <- turecover2$Quantile.rate
  N.RATEd <- c(N.RATEd,N.RATE)
  P.RATEd <- c(P.RATEd,P.RATE)
  Q.RATEd <- c(Q.RATEd,Q.RATE) 
  
}
plot(N,N.RATEd,ylab= '真实覆盖率',type='b',col='red',xaxt='n',ylim = c(0,0.4),main='真实覆盖率变化图（固定重抽样次数，变化样本数）')
axis(side = 1,at=c(50,100,200),labels =c(50,100,200))
points(N,P.RATEd,type='b',col ='GREEN')
points(N,Q.RATEd,type='b',col = 'blue')
legend("bottomright",legend=c("正态","枢轴量","分位数"),col=c("red","green","blue"),lwd=2,bty = "o",xjust = 0, yjust = 1, x.intersp = 1, y.intersp =1, adj = c(0, 0.5), text.width = NULL,xpd=TRUE)
})
##  用户  系统  流逝 
## 91.11  0.22 93.03 

debug(Truecoveragerate)
Truecoveragerate(50,1000,1000)
undebug(Truecoveragerate)

Rprof()
Truecoveragerate(50,1000,1000)
Rprof(NULL)
summaryRprof()

profvis({
    N.RATEd <- NULL
    P.RATEd <- NULL
    Q.RATEd <- NULL
    N <- c(50,100,200)
    for(i in 1:3)
    {
      n <- N[i]   
      turecover2 <- Truecoveragerate(n,1000,1000)
      N.RATE <- turecover2$NORM.rate
      P.RATE <- turecover2$PIVOTAL.rate
      Q.RATE <- turecover2$Quantile.rate
      N.RATEd <- c(N.RATEd,N.RATE)
      P.RATEd <- c(P.RATEd,P.RATE)
      Q.RATEd <- c(Q.RATEd,Q.RATE) 
      
    }
    plot(N,N.RATEd,ylab= '真实覆盖率',type='b',col='red',xaxt='n',ylim = c(0,0.4),main='真实覆盖率变化图（固定重抽样次数，变化样本数）')
    axis(side = 1,at=c(50,100,200),labels =c(50,100,200))
    points(N,P.RATEd,type='b',col ='GREEN')
    points(N,Q.RATEd,type='b',col = 'blue')
    legend("bottomright",legend=c("正态","枢轴量","分位数"),col=c("red","green","blue"),lwd=2,bty = "o",xjust = 0, yjust = 1, x.intersp = 1, y.intersp =1, adj = c(0, 0.5), text.width = NULL,xpd=TRUE)
})
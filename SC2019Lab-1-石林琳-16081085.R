#(1)读入数据
read.csv('C:/Users/Por/Desktop/R/swimming_pools.csv')
read.table('C:/Users/Por/Desktop/R/swimming_pools.csv',sep=',',header=T)

#(2)写数据
df<-data.frame(
  Name=c("Andrew Charlton Pool","Cairns Esplanade Lagoon"),
  Address=c("1C Mrs Macquaies Rd","Lot 16 Esplanade"),
  Latitude=c(-27.12345,-27.23456),
  Longitude=c(153.78912,153.45678)
)
#在原数据文件后增加数据
write.table(df,'C:/Users/Por/Desktop/R/swimming_pools.csv',
            append=TRUE,sep=',',row.names=FALSE,col.names=FALSE)
#将数据复制，保存为R文件
save(df,file='C:/Users/Por/Desktop/R/swimming_pools.Rdata')
dput(df,file='C:/Users/Por/Desktop/R/swimming_pools_append.csv')

#(3)重新读入数据
x<-read.csv('C:/Users/Por/Desktop/R/swimming_pools.csv')
x

#(4)提取子集
x[21,]#提取第21行元素
x[21,3]#提取第21行的第3个元素
x[[2]]#提取第2列元素
x[,2]#同上
x[["Address"]]#提取变量名为"Address"的元素
x$Address#同上
x[c(3,4)]#提取第3列和第4列元素
x[[c(3,4)]]#提取第3列的第4个元素
x[[3]][[4]]#同上

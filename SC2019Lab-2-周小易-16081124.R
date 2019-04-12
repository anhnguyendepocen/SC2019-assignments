####统计计算第二次作业--16081124--周小易
# In this lab, you will use the temperature data in four cities:
# Melbourne, Sydney,Brisbane and Cairns.

# 1. Please make a function load.file() to read a .csv file and transform the first
# column (a character representing date and time) using as.POSIXlt into R time
# format.
load.file<-function(filename){
  dataname<-read.csv(file = filename)
  dataname$time <- as.POSIXlt(dataname$time,format="%Y-%m-%d %H:%M:%S")
  #不能用dataframe[n]取值?
  dataname
}

# 2. Then apply load.file() to each filename using lapply().
list.files()

datalist<-lapply(c("Brisbane.csv", "Cairns.csv", "Melbourne.csv", "Sydney.csv"),load.file)

# 3. How many rows of data are there for each city?
sapply(datalist, nrow)

##99 for Brisbane
##80 for Cairns
##97 for Melbourne
##99 for Sydney

# 4. What is the hottest temperature recorded by city?
sapply(datalist[c(1:4)],function(data){
  max(data[,4])
})
# 23.89 35.00 13.33 23.33

# 5. Estimate the autocorrelation function for each city.
sapply(datalist[c(1:4)], function(data){
  acf(data[,2])
})














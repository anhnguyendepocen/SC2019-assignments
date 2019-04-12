###SC2019Lab-2-朱帅东-16081133

#Part I: Make function
load.file <- function(file,header = TRUE, sep = ",",...){
  data <- read.csv(file,header = header,sep = sep, ...)
  data$time <- as.POSIXlt(data[,1])
  return(data)
}
#Part II: Apply function 
file_set <- list(Melbourne = "G:/大三下学期/统计计算/作业2/temp/Melbourne.csv", 
                 Sydney = "G:/大三下学期/统计计算/作业2/temp/Sydney.csv", 
                 Brisbane = "G:/大三下学期/统计计算/作业2/temp/Brisbane.csv",
                 Cairns = "G:/大三下学期/统计计算/作业2/temp/Cairns.csv")
final_data <- lapply(file_set,load.file)
final_data

#Part III: Calculate the number of rows each city's data
sapply(final_data,nrow)

#Part IV: Get the hottest temperature recorded by city
sapply(final_data,function(data){
  max(data[,"temp.max"])
})

#Part V: Estimate the autocorrelation function for each city
ACFEstimate <- function(data){
  return(acf(data$temp)$acf)
}
sapply(final_data,ACFEstimate)

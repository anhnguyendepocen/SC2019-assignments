#L4
setwd("C:\\Users\\Administrator\\Desktop")
library(dplyr)
profvis({
  News <- readLines("BABAnews.txt")
  Encoding(News)<-'UTF-8'#Set the Declared Encodings for a Character Vector as 'UTF-8' 
  length(News)#the number of paragraphs is 5
  News <- trimws(News,which = c("left"))#	Remove Leading/Trailing Whitespace
  sum(nchar(News))# the number of characters is 967
  News <-paste(News,collapse=" ")#Collapse paragraphs into one
  unlist(News)#display it on the screen
  grepl('技术架构', News)#true
  News <-gsub('双11|双 11', '双十一', News)#Replace '双11' with '双十一'.
  r <- gregexpr("(.*?)。", News)
  News <- regmatches(News, r)#Split the article into sentences by periods
})
News

#L5

#To find the error:do not know how to convert 'time' to class <U+00A1><U+00B0>POSIXlt<U+00A1><U+00B1>.
  load.file <- function(data) {
    pools<- read.csv(data)#use 20
    pools$time<-as.POSIXlt(pools$time)#use 20
    pools <- mutate(pools, time = as.POSIXlt(time))
    pools
  }
  data<-lapply(c('Brisbane.csv','Cairns.csv','Melbourne.csv','Sydney.csv'),load.file)#use 50
  max.file<-function(d)
  {
    data.frame(d)
    max(d$temp.max)
  }
  acf.file<-function(d)
  {
    data.frame(d)
    acflist<-lapply(d[,c(2:4)],acf)
    data.frame(sapply(acflist,function(acf){acf$acf}))
  }
  lapply(data,nrow)#rows
  lapply(data, max.file)#hottest temp
  lapply(data,acf.file)#autocorrelation function
  
  
  traceback()#prints out the function call stack 
  #Error: Column `time` is a date/time and must be stored as POSIXct, not POSIXlt.
  #Call `rlang::last_error()` to see a backtrace 
  #14.stop(cnd) 
  #13.abort(error_time_column_must_be_posixct(names_x[posixlt])) 
  #12.check_valid_cols(x) 
  #11.lst_to_tibble(x, .rows, .name_repair, col_lengths(x)) 
  #10.as_tibble.list(unclass(x), ..., .rows = .rows, .name_repair = .name_repair) 
  #9.as_tibble.data.frame(data, .name_repair = "check_unique") 
  #8.as_tibble(data, .name_repair = "check_unique") 
  #7.tbl_df(.data) 
  #6.mutate(tbl_df(.data), ...) 
  #5.as.data.frame(mutate(tbl_df(.data), ...)) 
  #4.mutate.data.frame(pools, time = as.POSIXlt(time)) 
  #3.mutate(pools, time = as.POSIXlt(time)) 
  #2.FUN(X[[i]], ...) 
  #1.lapply(c("Brisbane.csv", "Cairns.csv", "Melbourne.csv", "Sydney.csv"), load.file) 
  debug(max.file)
  lapply(data, max.file)
  options(error = recover)
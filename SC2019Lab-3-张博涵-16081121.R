#################
# Lab Session 3 #
#################

# 1.
alinews <- readLines("Lab3/BABAnews.txt", encoding = "UTF-8")
alinews

# 2.
length(alinews)
## 5

# 3.
alinews <- trimws(alinews)

# 4.
sum(nchar(alinews))

# 5.
alinews2 <- paste(alinews, collapse = "")
alinews2

# 6.
grepl("技术架构", alinews2)
## TRUE

# 7.
alinews3 <- strsplit(alinews2, split = "。|。”|？")
alinews3
# 8.
gsub("双11|双 11 ","双十一", alinews)
## replace both 双11 and 双 11 

#################
# Lab Session 4 #
#################
# function
Bootstarp_sample <- function(data, FUN, ...,m = floor(length(data)*0.7),B = 10000){
  funs <- NULL
  for(i in 1:B){
    sample.x <- sample(data, m, replace = T)
    sample.fun <- FUN(sample.x, ...)
    funs <- c(funs, sample.fun)
  }
  return(funs)
}

debug(Bootstarp_sample)
Rprof()
a <- Bootstarp_sample(flights$sched_dep_time, var) # data from flights
Rprof(NULL)
summaryRprof()



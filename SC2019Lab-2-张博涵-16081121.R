library(dplyr)
# Practice 1
Save.file <- function(file_path){
  f <- read.csv(file_path)
  f[[1]] <- as.POSIXlt(f[,1]) #here use f[,1] dont return true results 
  # Or use as.POSIXct
  return(f)
}

# Practice 2
files <- list(Sydney = "Lab2/temp/Sydney.csv", Melourne = "Lab2/temp/Melbourne.csv",Cairns = "Lab2/temp/Cairns.csv",Brisbane = "Lab2/temp/Brisbane.csv")
temp <- lapply(files, Save.file)

# Practice 3
sapply(temp, nrow)

## Sydney Melourne   Cairns Brisbane 
## 99       97       80       99 

# Practice 4
max_temp <- temp %>% sapply(select,temp.max) %>% sapply(max) %>% max
## [1] 35

# Practice 5
acf_temp <- temp %>% sapply(select,temp) %>% lapply(acf, plot = FALSE)


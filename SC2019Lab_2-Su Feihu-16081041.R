###SC2019 Lab Session 2
##name:Su Feihu  StudentID:16081041

##Please make a function load.file() to read a .csv file 
##and transform the first column (a character representing date and time) 
##using as.POSIXlt into R time format.
load.file <- function(file){
  files <- read.csv(file, header = TRUE)
  files[, 1] <- as.POSIXlt(files$time)
  files
}


##apply load.file() to each filename using lapply().
#create a list to save files of dates
cityfiles <- list("D:/study/dateset/statistic computing/temp/Brisbane.csv",
                 "D:/study/dateset/statistic computing/temp/Cairn.csv",
                 "D:/study/dateset/statistic computing/temp/Melbourne.csv",
                 "D:/study/dateset/statistic computing/temp/Sydney.csv")

#use lapply() to load each files
temps <- lapply(cityfiles, load.file)
citynames <- c('Brisbane', 'Cairns', 'Melbourne', 'Sydney')
names(temps) <- citynames

##How many rows of data are there for each city?
sapply(temps, nrow)

##What is the hottest temperature recorded by city?
maxtemp <- function(elt){
  max(elt[, 4])
}
  lapply(temps, maxtemp)

##Estimate the autocorrelation function for each city.
acftemp <- function(elt){
    acf(ts(elt[ ,2]))
  }
lapply(Temps, acftemp)

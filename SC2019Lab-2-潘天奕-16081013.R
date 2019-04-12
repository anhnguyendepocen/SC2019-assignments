#Write function to load csvfile and transform the first col to time format
load.file <- function(filename, col = 1){
  fdata <- read.csv(filename)
  fdata[, col] <- list(as.POSIXlt(fdata[, col]))
  fdata 
}

#Create a list to save files
CityName <- c('Brisbane', 'Cairns', 'Melbourne', 'Sydney')
City <- lapply(CityName, function(elt){
  paste("D:/statistic computing/Data/temp/", elt, ".csv", sep = '')
})

#List TempCity for the 4 cities 
TempCity <- lapply(City, load.file)
names(TempCity) <- CityName

#How many rows of data are there for each city
lapply(TempCity, nrow)

#What is the hottest temperature recorded by city?
lapply(TempCity, function(elt){max(elt$temp)})

#Estimate the autocorrelation function for each city.
lapply(TempCity, acf)
par(mfcol=c(2,2))
mapply(function(elt1, elt2){acf(elt1$temp, main = elt2)}, TempCity, CityName, SIMPLIFY = F)

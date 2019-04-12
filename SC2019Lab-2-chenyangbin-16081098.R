#Please return all the rows where Temp is larger than 80 and Month is after May.
library(dplyr)
airquality
da1<-filter(airquality,airquality$Temp>80 & airquality$Month>5)
da1


#Please add a new column that displays the temperature in Celsius
air2<-airquality
air2<-mutate(air2,CelsiusTemp = (air2$Temp-32)*5/9)
air2



#Calculate the mean temperature in each month.
da2<-group_by(airquality,airquality$Month)
da2
summarize(da2, t = mean(Temp, na.rm = TRUE))


#Remove all the data corresponding to Month = 5, 
#group the data by month, and then find the mean of the temperature each month.
air3<-filter(airquality,airquality$Month>5)
air3
da3<-group_by(air3,air3$Month)
da3
summarize(da3, t = mean(Temp, na.rm = TRUE))
          
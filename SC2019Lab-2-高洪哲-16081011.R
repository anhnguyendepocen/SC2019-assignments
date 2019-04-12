setwd("D:/ProgramProject/R/Lab2")
library(datasets)
library(dplyr)
airquality

filter(airquality,Temp>80&Month>5)
options(digits = 3) #三位有效数字
#use %>%
mutate(airquality, Celsius=(Temp-32)/1.8) %>%
group_by(Month)  %>%
summarize(mean(Temp)) %>%
filter(Month!=5)

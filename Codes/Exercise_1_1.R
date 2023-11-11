setwd("C:/Users/510908/Documents/code/portfolio-management/Data")
#import data 
ret = read.csv("RET.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
factors = read.csv("F-F_Research_Data_Factors_daily.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(ret)
View(factors)


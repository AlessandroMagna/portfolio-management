### IMPORT LIRARIES ###

### IMPORT DATA ###
setwd("C:/Users/510908/Documents/code/portfolio-management/Data")
#import data 
ret = read.csv("RET.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(ret)
factors = read.csv("F-F_Research_Data_Factors_daily.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(factors)

### PREPROCESSING DATA ###
#format factors$X into a date format with YYYY-MM-DD
factors$X = as.Date(factors$X, format = "%Y%m%d")

#find the index of factors where factors$x is 2010-01-04
start = which(factors$X == "2010-01-04")
end = which(factors$X == "2016-12-30")

#subset factors to only include rows between start and end
factors = factors[start:end,]

#reset index for factors
rownames(factors) = NULL



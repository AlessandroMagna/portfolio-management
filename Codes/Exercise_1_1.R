### IMPORT PACKAGES ###
set.seed(646425)
library("psych")
library("MASS")


### IMPORT DATA ###
setwd("C:/Users/510908/Documents/code/portfolio-management/Data")
#import data 
ret_raw = read.csv("RET.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(ret_raw)
factors_raw = read.csv("F-F_Research_Data_Factors_daily.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(factors_raw)

### PREPROCESSING DATA ###
#format factors$X into a date format with YYYY-MM-DD
factors_raw$X = as.Date(factors_raw$X, format = "%Y%m%d")

#find the index of factors where factors$x is 2010-01-04
start = which(factors_raw$X == "2010-01-04")
end = which(factors_raw$X == "2016-12-30")

#subset factors to only include rows between start and end
factors_raw = factors_raw[start:end,]

#reset index for factors
rownames(factors_raw) = NULL

#turn ret into a matrix
ret_raw = as.matrix(ret_raw)
#remove column DATE from ret
ret = ret_raw[,-1]
ret_matrix = as.matrix(as.numeric(ret))
#transfrorm ret_matrix into a matrix with 543 columns and 1762 rows
ret_matrix = matrix(ret_matrix, nrow = 1762, ncol = 543)
typeof(ret_matrix)
#add header to ret_matrix
colnames(ret_matrix) = colnames(ret_raw)[-1]

#remove column X from factors
factors = factors_raw[, -1]
factor_matrix = as.matrix(factors)

mkt_factor = as.matrix(factor_matrix[,1])

#REGRESSION
# Create an empty data frame to store regression coefficients
coefficients_table <- data.frame(Asset = character(), Intercept = numeric(), Beta = numeric(), stringsAsFactors = FALSE)

# Loop through each asset and perform regression
for (i in 1:ncol(ret_matrix)) {
  # Extract returns for the current asset
  asset_returns <- ret_matrix[, i]
  # Perform linear regression
  regression_result <- lm(asset_returns ~ mkt_factor)
  # Store coefficients in the table
  coefficients_table <- rbind(coefficients_table, c(paste(i), coef(regression_result)[1], coef(regression_result)[2]))
}
# Rename columns in the coefficients table
colnames(coefficients_table) <- c("Asset", "Intercept", "Beta")
# Print the coefficients table
View(coefficients_table)

#plot intercept
plot(coefficients_table$Beta, type = "l", col = "blue", xlab = "Asset", ylab = "Intercept", main = "Intercept for each asset")

#give me some descriptive statistics for coefficients_table$Intercept and coefficients_table$Beta
describe(as.numeric(coefficients_table$Intercept))
describe(as.numeric(coefficients_table$Beta))



#################
# Exercise 1.b #
################

sharpe_mkt = mean(mkt_factor)/sd(mkt_factor)
sharpe_mkt

#################
# Exercise 1.c #
################

#randomly pick 10 numbers between 1 and 543
random = sample(1:543, 10)
random
#create a matrix with 10 columns containing columns of ret_matrix with the index of random
ten_assets_matrix = ret_matrix[,random]
View(ten_assets_matrix)
#calculate the sample mean and variance of ten_assets_matrix (and consider these the TRUE parameters of the DGP)
ten_sample_mean = as.matrix(colMeans(ten_assets_matrix))
ten_sample_cov = cov(ten_assets_matrix)
View(ten_sample_cov)

#simulate 20000 observations from a multivariate normal distribution with mean ten_sample_mean and covariance ten_sample_cov
simulated = mvrnorm(20000, ten_sample_mean, ten_sample_cov)
View(simulated)

#add a column to simulated containing the mean of each row named "mean"
simulated = cbind(simulated, rowMeans(simulated))
colnames(simulated)[11] = "EW Returns"

#calculate the sharpe ratio of EW Returns
EW_sharpe = mean(simulated[,11])/sd(simulated[,11])
EW_sharpe









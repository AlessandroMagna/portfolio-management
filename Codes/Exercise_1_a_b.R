### IMPORT PACKAGES ###
set.seed(646425)
library("psych")
library("MASS")


### IMPORT DATA ###
#setwd("C:/Users/macie/Desktop/uni/PM assignment/portfolio-management-main/Data")
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
# simulation 10 #
#################

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
colnames(simulated)[11] = "EW Returns 10 assets"

############################
# Exercise 1.c (10 assets) #
############################

#calculate the sharpe ratio of EW Returns
EW_sharpe = mean(simulated[,11])/sd(simulated[,11])
EW_sharpe

#for the turnover of the EW portfolio, calculate turnover as the average (ovet T) sum of absolute changes in weights of assets, between the desired weight (1/10), and realised weight of the asset, being the previous weight multiplied by the asset return and divided by return of the portfolio, summed over the portfolio
#calculate the turnover of the EW portfolio
EW_turnover = 0
N = 10
T = 20000
for (i in 1:T) {
  for (j in 1:N) {
    EW_turnover = EW_turnover + abs((1/N) - ((1/N)*simulated[i,j]/simulated[i,11]))
  }
}
EW_turnover = EW_turnover/T
EW_turnover

############################
# Exercise 1.d (10 assets) #
############################



#computing tangency weights
N = 10
M = 3600
iota = matrix(1, nrow = N , ncol = 1)
tangency_10 = matrix(0, nrow = 20000, ncol = N)
for (i in (M+1):20000) {
  cov = cov(simulated[(i-M):i,1:N])
  mean = colMeans(simulated[(i-M):i,1:N])
  denominator = t(iota) %*% solve(cov) %*% mean
  tangency = solve(cov) %*% mean / denominator[1,1]
  tangency_10[i,] = tangency
}


tngReturns_10 = matrix(0, nrow = 20000, ncol = 1)
for (i in 1:20000) {
  tngReturns_10[i,1] = t(tangency_10[i,]) %*% simulated[i,1:N]
}


#calculate the turnover of the tangency portfolio
tangency_turnover = 0
T = 20000
for (i in (M+1):T) {
  for (j in 1:N) {
    tangency_turnover = tangency_turnover + abs(tangency_10[i,j] - tangency_10[i-1,j]*simulated[i,j]/tngReturns_10[i,])
  }
}
tangency_turnover = tangency_turnover/(T-M)
tangency_turnover

#calculate the sharpe ratio of tangency portfolio
tangency_sharpe = mean(tngReturns_10[(M+1):T,])/sd(tngReturns_10[(M+1):T,])
tangency_sharpe



#################
# simulation 100 #
#################

#randomly pick 100 numbers between 1 and 543
random = sample(1:543, 100)
random
#create a matrix with 100 columns containing columns of ret_matrix with the index of random
hun_assets_matrix = ret_matrix[,random]
View(hun_assets_matrix)
#calculate the sample mean and variance of hun_assets_matrix (and consider these the TRUE parameters of the DGP)
hun_sample_mean = as.matrix(colMeans(hun_assets_matrix))
hun_sample_cov = cov(hun_assets_matrix)
View(hun_sample_cov)

#simulate 20000 observations from a multivariate normal distribution with mean hun_sample_mean and covariance hun_sample_cov
simulated100 = mvrnorm(20000, hun_sample_mean, hun_sample_cov)
View(simulated100)

#add a column to simulated100 containing the mean of each row named "mean"
simulated100 = cbind(simulated100, rowMeans(simulated100))
colnames(simulated100)[101] = "EW Returns 100 assets"

############################
# Exercise 1.c (100 assets) #
############################

#calculate the sharpe ratio of EW Returns
EW_sharpe = mean(simulated100[,101])/sd(simulated100[,101])
EW_sharpe

#for the turnover of the EW portfolio, calculate turnover as the average (ovet T) sum of absolute changes in weights of assets, between the desired weight (1/100), and realised weight of the asset, being the previous weight multiplied by the asset return and divided by return of the portfolio, summed over the portfolio
#calculate the turnover of the EW portfolio
EW_turnover = 0
N = 100
T = 20000
for (i in 1:T) {
  for (j in 1:N) {
    EW_turnover = EW_turnover + abs((1/N) - ((1/N)*simulated100[i,j]/simulated100[i,101]))
  }
}
EW_turnover = EW_turnover/T
EW_turnover

############################
# Exercise 1.d (100 assets) #
############################



#computing tangency weights
N = 100
M = 3600
iota = matrix(1, nrow = N , ncol = 1)
tangency_100 = matrix(0, nrow = 20000, ncol = N)
for (i in (M+1):20000) {
  cov = cov(simulated100[(i-M):i,1:N])
  mean = colMeans(simulated100[(i-M):i,1:N])
  denominator = t(iota) %*% solve(cov) %*% mean
  tangency = solve(cov) %*% mean / denominator[1,1]
  tangency_100[i,] = tangency
}

tngReturns_100 = matrix(0, nrow = 20000, ncol = 1)
for (i in 1:20000) {
  tngReturns_100[i,1] = t(tangency_100[i,]) %*% simulated100[i,1:N]
}

#calculate the turnover of the tangency portfolio
tangency_turnover = 0
N = 100
T = 20000
for (i in (M+1):T) {
  for (j in 1:N) {
    tangency_turnover = tangency_turnover + abs(tangency_100[i,j] - tangency_100[i-1,j]*simulated100[i,j]/tngReturns_100[i,])
  }
}
tangency_turnover = tangency_turnover/(T-M)
tangency_turnover

#calculate the sharpe ratio of tangency portfolio
tangency_sharpe = mean(tngReturns_100[(M+1):T,])/sd(tngReturns_100[(M+1):T,])
tangency_sharpe


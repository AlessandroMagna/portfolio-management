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
simulated_10 = mvrnorm(20000, ten_sample_mean, ten_sample_cov)
View(simulated_10)

#add a column to simulated_10 containing the mean of each row named "mean"
simulated_10 = cbind(simulated_10, rowMeans(simulated_10))
colnames(simulated_10)[11] = "EW Returns 10 assets"

##################
# simulation 100 #
##################

#randomly pick 10 numbers between 1 and 543
random = sample(1:543, 100)
random
#create a matrix with 100 columns containing columns of ret_matrix with the index of random
hun_assets_matrix = ret_matrix[,random]
View(hun_assets_matrix)
#calculate the sample mean and variance of ten_assets_matrix (and consider these the TRUE parameters of the DGP)
hun_sample_mean = as.matrix(colMeans(hun_assets_matrix))
hun_sample_cov = cov(hun_assets_matrix)
View(hun_sample_cov)

#simulate 20000 observations from a multivariate normal distribution with mean ten_sample_mean and covariance ten_sample_cov
simulated_100 = mvrnorm(20000, hun_sample_mean, hun_sample_cov)
View(simulated_100)

#add a column to simulated_10 containing the mean of each row named "mean"
simulated_100 = cbind(simulated_100, rowMeans(simulated_100))

############################
# Exercise 1.c (10 assets) #
############################

#calculate the sharpe ratio of EW Returns
EW_sharpe = mean(simulated_10[,11])/sd(simulated_10[,11])
EW_sharpe

#for the turnover of the EW portfolio, calculate turnover as the average (ovet T) sum of absolute changes in weights of assets, between the desired weight (1/10), and realised weight of the asset, being the previous weight multiplied by the asset return and divided by return of the portfolio, summed over the portfolio
#calculate the turnover of the EW portfolio
EW_turnover = 0
N = 10
T = 20000
for (i in 1:T) {
  for (j in 1:N) {
    EW_turnover = EW_turnover + abs((1/N) - ((1/N)*simulated_10[i,j]/simulated_10[i,11]))
  }
}
EW_turnover = EW_turnover/T
EW_turnover

'''
############################
# Exercise 1.d (10 assets) #
############################
'''

calculate_portfolio_metrics_TAN <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  TAN <- matrix(0, nrow = T, ncol = N)
  tanReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate TAN weights and TAN Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):i, 1:N])
    mean = colMeans(simul[(i-M):i,1:N])
    denominator <- t(iota) %*% solve(cov_matrix) %*% mean
    tan <- solve(cov_matrix) %*% mean / denominator[1, 1]
    TAN[i,] <- tan
    tanReturns[i,1] <- t(tan) %*% simul[i, 1:N]
  }
  
  # Calculate turnover
  tan_turnover <- 0
  for (i in (M + 1):T) {
    for (j in 1:N) {
      tan_turnover = tan_turnover + abs(TAN[i, j] - TAN[i - 1, j] * simul[i, j] / tanReturns[i,])
    }
  }
  tan_turnover <- tan_turnover / (T - M)
  
  # Calculate Sharpe ratio
  tan_sharpe <- mean(tanReturns[(M + 1):T, ]) / sd(tanReturns[(M + 1):T, ])
  
  return(list(weights = TAN, sharpe_ratio = tan_sharpe, turnover = tan_turnover))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_10_120 <- calculate_portfolio_metrics_TAN(10, 120, simulated_data)
TAN_sharpe_ratio_10_120 <- TAN_result_10_120$sharpe_ratio
TAN_turnover_10_120 <- TAN_result_10_120$turnover

TAN_result_10_240 <- calculate_portfolio_metrics_TAN(10, 240, simulated_data)
TAN_sharpe_ratio_10_240 <- TAN_result_10_240$sharpe_ratio
TAN_turnover_10_240 <- TAN_result_10_240$turnover

TAN_result_10_3600 <- calculate_portfolio_metrics_TAN(10, 3600, simulated_data)
TAN_sharpe_ratio_10_3600 <- TAN_result_10_3600$sharpe_ratio
TAN_turnover_10_3600 <- TAN_result_10_3600$turnover

########
'N = 100'
########

N <- 100
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_100_120 <- calculate_portfolio_metrics_TAN(100, 120, simulated_data)
TAN_sharpe_ratio_100_120 <- TAN_result_100_120$sharpe_ratio
TAN_turnover_100_120 <- TAN_result_100_120$turnover

TAN_result_100_240 <- calculate_portfolio_metrics_TAN(100, 240, simulated_data)
TAN_sharpe_ratio_100_240 <- TAN_result_100_240$sharpe_ratio
TAN_turnover_100_240 <- TAN_result_100_240$turnover

TAN_result_100_3600 <- calculate_portfolio_metrics_TAN(100, 3600, simulated_data)
TAN_sharpe_ratio_100_3600 <- TAN_result_100_3600$sharpe_ratio
TAN_turnover_100_3600 <- TAN_result_100_3600$turnover

'''
############################
# Exercise 1.e (10 assets) # (TAN in sample)
############################
'''

calculate_portfolio_metrics_TAN_in_sample <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  TAN <- matrix(0, nrow = T, ncol = N)
  tanReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate TAN weights and TAN Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):i, 1:N])
    mean = colMeans(simul[(i-M):i,1:N])
    denominator <- t(iota) %*% solve(cov_matrix) %*% mean
    tan <- solve(cov_matrix) %*% mean / denominator[1, 1]
    TAN[i,] <- tan
    tanReturns[i,1] <- t(tan) %*% simul[i, 1:N]
  }
  
  # Calculate turnover
  tan_turnover <- 0
  for (i in (M + 1):T) {
    for (j in 1:N) {
      tan_turnover = tan_turnover + abs(TAN[i, j] - TAN[i - 1, j] * simul[i, j] / tanReturns[i,])
    }
  }
  tan_turnover <- tan_turnover / (T - M)
  
  # Calculate Sharpe ratio
  tan_sharpe <- mean(tanReturns[(M + 1):T, ]) / sd(tanReturns[(M + 1):T, ])
  
  return(list(weights = TAN, sharpe_ratio = tan_sharpe, turnover = tan_turnover))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_10_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, 120, simulated_data)
TAN_sharpe_ratio_10_120_in_sample <- TAN_result_10_120_in_sample$sharpe_ratio
TAN_turnover_10_120_in_sample <- TAN_result_10_120_in_sample$turnover

TAN_result_10_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, 240, simulated_data)
TAN_sharpe_ratio_10_240_in_sample <- TAN_result_10_240_in_sample$sharpe_ratio
TAN_turnover_10_240_in_sample <- TAN_result_10_240_in_sample$turnover

TAN_result_10_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, 3600, simulated_data)
TAN_sharpe_ratio_10_3600_in_sample <- TAN_result_10_3600_in_sample$sharpe_ratio
TAN_turnover_10_3600_in_sample <- TAN_result_10_3600_in_sample$turnover

########
'N = 100'
########

N <- 100
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_100_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, 120, simulated_data)
TAN_sharpe_ratio_100_120_in_sample <- TAN_result_100_120_in_sample$sharpe_ratio
TAN_turnover_100_120_in_sample <- TAN_result_100_120_in_sample$turnover

TAN_result_100_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, 240, simulated_data)
TAN_sharpe_ratio_100_240_in_sample <- TAN_result_100_240_in_sample$sharpe_ratio
TAN_turnover_100_240_in_sample <- TAN_result_100_240_in_sample$turnover

TAN_result_100_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, 3600, simulated_data)
TAN_sharpe_ratio_100_3600_in_sample <- TAN_result_100_3600_in_sample$sharpe_ratio
TAN_turnover_100_3600_in_sample <- TAN_result_100_3600_in_sample$turnover


'''
###########################################
# Exercise 1.f (10 assets)  GMV portfolio #
###########################################
'''

calculate_portfolio_metrics_GMV <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  GMV <- matrix(0, nrow = T, ncol = N)
  gmvReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate GMV and GMV Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):i, 1:N])
    denominator <- t(iota) %*% solve(cov_matrix) %*% iota
    gmv <- solve(cov_matrix) %*% iota / denominator[1, 1]
    GMV[i,] <- gmv
    gmvReturns[i,1] <- t(gmv) %*% simul[i, 1:N]
  }
  
  # Calculate turnover
  gmv_turnover <- 0
  for (i in (M + 1):T) {
    for (j in 1:N) {
      gmv_turnover = gmv_turnover + abs(GMV[i, j] - GMV[i - 1, j] * simul[i, j] / gmvReturns[i,])
    }
  }
  gmv_turnover <- gmv_turnover / (T - M)
  
  # Calculate Sharpe ratio
  gmv_sharpe <- mean(gmvReturns[(M + 1):T, ]) / sd(gmvReturns[(M + 1):T, ])
  
  return(list(weights = GMV, sharpe_ratio = gmv_sharpe, turnover = gmv_turnover))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]
  
GMV_result_10_120 <- calculate_portfolio_metrics_GMV(10, 120, simulated_data)
GMV_sharpe_ratio_10_120 <- GMV_result_10_120$sharpe_ratio
GMV_turnover_10_120 <- GMV_result_10_120$turnover

GMV_result_10_240 <- calculate_portfolio_metrics_GMV(10, 240, simulated_data)
GMV_sharpe_ratio_10_240 <- GMV_result_10_240$sharpe_ratio
GMV_turnover_10_240 <- GMV_result_10_240$turnover

GMV_result_10_3600 <- calculate_portfolio_metrics_GMV(10, 3600, simulated_data)
GMV_sharpe_ratio_10_3600 <- GMV_result_10_3600$sharpe_ratio
GMV_turnover_10_3600 <- GMV_result_10_3600$turnover

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

GMV_rGMV_esult_100_120 <- calculate_portfolio_metrics_GMV(100, 120, simulated_data)
GMV_sharpe_ratio_100_120 <- GMV_result_100_120$sharpe_ratio
GMV_turnover_100_120 <- GMV_result_100_120$turnover

GMV_result_100_240 <- calculate_portfolio_metrics_GMV(100, 240, simulated_data)
GMV_sharpe_ratio_100_240 <- GMV_result_100_240$sharpe_ratio
GMV_turnover_100_240 <- GMV_result_100_240$turnover

GMV_result_100_3600 <- calculate_portfolio_metrics_GMV(100, 3600, simulated_data)
GMV_sharpe_ratio_100_3600 <- GMV_result_100_3600$sharpe_ratio
GMV_turnover_100_3600 <- GMV_result_100_3600$turnover

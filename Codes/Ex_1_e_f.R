### IMPORT PACKAGES ###
set.seed(646425)
library("psych")
library("MASS")
library("zoo")

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


################
# Exercise 1.e #
################
# Function to calculate the covariance matrix for a given window
calculate_covariance_matrix <- function(window_matrix) {
  cov_matrix <- cov(window_matrix)
  return(cov_matrix)
}

calculate_GMV_results <- function(N, M) {
  # Create 1 vector of N ones
  iota <- matrix(c(rep(1, N)), nrow = N, ncol = 1)
  
  Sigma <- list()
  for (i in (M+1):20000) {
    Sigma[[i]] = calculate_covariance_matrix(simulated[(i - M):i, 1:N])
  }
  
  w_GMV <- list()
  GMV_moments <- matrix(data = NA, nrow = 20000, ncol = 2, dimnames = list(NULL, c("mean", "sd")))
  
  for (i in M:20000) {
    w_GMV[[i]] <- (solve(Sigma[[i]]) %*% iota) %*% (1 / ((t(iota) %*% solve(Sigma[[i]]) %*% iota)))
    
    if (!is.null(w_GMV[[i]])) {
      GMV_moments[i, 1] <- sum(w_GMV[[i]] * simulated[i, ])
      GMV_moments[i, 2] <- sqrt(t(w_GMV[[i]]) %*% Sigma[[i]] %*% w_GMV[[i]])
    } else {
      GMV_moments[i, 1] <- NA
      GMV_moments[i, 2] <- NA
    }
  }
  
  result_list <- list(w_gmv = w_GMV, gmv_moments = GMV_moments)
  return(result_list)
}

calculate_GMV_sharpe <- function(N, M) {
  # Call the first function to get w_GMV and GMV_moments
  result_list <- calculate_GMV_results(N, M)
  
  # Extract w_GMV and GMV_moments from the result_list
  w_GMV <- result_list$w_gmv
  GMV_moments <- result_list$gmv_moments
  
  # Calculate GMV_sharpe
  GMV_sharpe <- mean(GMV_moments[, 1], na.rm = TRUE) / sd(GMV_moments[, 1], na.rm = TRUE)
  return(GMV_sharpe)
}

#calculate turnover of weights
calculate_GMV_turnover <- function(N, M) {
  for(t in M:(20000-1)){
    sum_diff = 0
    sum_of_sum_diff = 0
    
    for(j in 1:N){
      desired = w_GMV[[t+1]][j]
      before_rebalancing = (w_GMV[[t]][j] * simulated[t+1,j]) / (sum(w_GMV[[t+1]] * simulated[t+1,]))
      sum_diff = sum_diff + abs(desired - before_rebalancing)
    }
    sum_of_sum_diff = sum_of_sum_diff + sum_diff
  }
  GMV_turnover = sum_of_sum_diff / (20000 - 1 - M)
  return(GMV_turnover)
}



# Call the functions with N and M

#GMV with N = 10 and M = 120
w_GMV_10_120 = calculate_GMV_results(10, 120)$w_gmv
GMV_moments_10_120 = calculate_GMV_results(10, 120)$gmv_moments
GMV_sharpe_10_120 = calculate_GMV_sharpe(10, 120)
GMV_sharpe_10_120
GMV_turnover_10_120 = calculate_GMV_turnover(10, 120)
GMV_turnover_10_120

#GMV with N = 10 and M = 240
w_GMV_10_240 = calculate_GMV_results(10, 240)$w_gmv
GMV_moments_10_240 = calculate_GMV_results(10, 240)$gmv_moments
GMV_sharpe_10_240 = calculate_GMV_sharpe(10, 240)
GMV_sharpe_10_240
GMV_turnover_10_240 = calculate_GMV_turnover(10, 240)
GMV_turnover_10_240

#GMV with N = 10 and M = 3600
w_GMV_10_3600 = calculate_GMV_results(10, 3600)$w_gmv
GMV_moments_10_3600 = calculate_GMV_results(10, 3600)$gmv_moments
GMV_sharpe_10_3600 = calculate_GMV_sharpe(10, 3600)
GMV_sharpe_10_3600
GMV_turnover_10_3600 = calculate_GMV_turnover(10, 3600)
GMV_turnover_10_3600

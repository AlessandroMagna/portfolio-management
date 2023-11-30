### IMPORT PACKAGES ###
set.seed(1234)
library("psych")
library("MASS")

setwd("C:/Users/510908/Documents/code/portfolio-management/Data")
### IMPORT DATA ###
#setwd("C:/Users/macie/Desktop/uni/PM assignment/portfolio-management-main/Data")
#import data 
ret_raw = read.csv("RET.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
factors_raw = read.csv("F-F_Research_Data_Factors_daily.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

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
RF = as.matrix(factor_matrix[,4])

#REGRESSION

# remove risk free rate from ret_matrix
ret_matrix = ret_matrix - c(RF)
# Create an empty data frame to store regression coefficients
coefficients_table <- data.frame(Asset = numeric(), Intercept = numeric(), Beta = numeric(), StdErr = numeric(), stringsAsFactors = FALSE)

# Loop through each asset and perform regression
for (i in 1:ncol(ret_matrix)) {
  # Extract returns for the current asset
  asset_returns <- ret_matrix[, i]
  # Perform linear regression
  regression_result <- lm(asset_returns ~ 1 + mkt_factor)
  #find mean of errors
  error_mean = mean(regression_result[["residuals"]])
  #find variance of errors
  error_var = var(regression_result[["residuals"]])
  # Store coefficients and error mean and variance in the coefficients table
  coefficients_table <- rbind(coefficients_table, c(paste(i), coef(regression_result)[1], coef(regression_result)[2], summary(regression_result)$coef[2, "Std. Error"], error_mean, error_var))
}
# Rename columns in the coefficients table
colnames(coefficients_table) <- c("Asset", "Intercept", "Beta", "StdErr", "ErrorMean", "ErrorVar")
# Print the coefficients table
View(coefficients_table)

#plot intercept
plot(coefficients_table$Beta, type = "l", col = "blue", xlab = "Asset", ylab = "Intercept", main = "Intercept for each asset")

#descriptive statistics for coefficients_table$Intercept and coefficients_table$Beta
describe(as.numeric(coefficients_table$Intercept))
describe(as.numeric(coefficients_table$Beta))

mean(as.numeric(coefficients_table$Intercept))
mean(as.numeric(coefficients_table$Beta))
mean(as.numeric(coefficients_table$StdErr))


#################
# Exercise 1.b #
################

sharpe_mkt = mean(mkt_factor)/sd(mkt_factor)
sharpe_mkt

### Simulation based on sample moments ###


##calculate the sample mean and variance of ret_matrix (and consider these the TRUE parameters of the DGP)
sample_mean = as.matrix(colMeans(ret_matrix))
sample_cov = cov(ret_matrix)
##simulate 20000 observations from a multivariate normal distribution with mean sample_mean and covariance sample_cov
simulated = mvrnorm(20000, sample_mean, sample_cov)
##randomly pick 10 numbers between 1 and 543
random_10 = sample(1:543, 10)
random_10
##create a matrix with 10 columns containing columns of simulated with the index of random_10
simulated_10 = simulated[,random_10]
##randomly pick 100 numbers between 1 and 543
random_100 = sample(1:543, 100)
random_100
##create a matrix with 100 columns containing columns of simulated with the index of random_100
simulated_100 = simulated[,random_100]



### simulation based on factor model ###

# find market factor mean
mkt_factor_mean = mean(mkt_factor)
# find market factor variance
mkt_factor_var = var(mkt_factor)

#simulate
simulated = matrix(0, nrow = 20000, ncol = 543)
for (i in 1:543) {
  for (j in 1:20000) {
    beta_value = as.numeric(coefficients_table$Beta[i])
    error_mean = as.numeric(coefficients_table$ErrorMean[i])
    error_var = as.numeric(coefficients_table$ErrorVar[i])
    #simulated[j, i] = rnorm(1, mean = mkt_factor_mean * beta_value, sd = sqrt(mkt_factor_var[1, 1] * beta_value^2 + error_var))
    simulated[j, i] = beta_value*rnorm(1,mkt_factor_mean, sqrt(mkt_factor_var)) + rnorm(1, error_mean, sqrt(error_var))
  }
}

#randomly pick 10 numbers between 1 and 543
random_10 = sample(1:543, 10)
random_10
#create a matrix with 10 columns containing columns of simulated with the index of random_10
simulated_10 = simulated[,random_10]
#randomly pick 100 numbers between 1 and 543
random_100 = sample(1:543, 100)
random_100
#create a matrix with 100 columns containing columns of simulated with the index of random_100
simulated_100 = simulated[,random_100]

#create 2 matrices where I will store all the Sharpe Ratios and Turnovers for all the configurations
#Sharpe_Ratios_2 = Sharpe_Ratios
Sharpe_Ratios = matrix(NA, nrow = 6, ncol = 6)
colnames(Sharpe_Ratios) = c("10 - 120", "10 - 240", "10 - 3600", "100 - 120", "100 - 240", "100 - 3600")
rownames(Sharpe_Ratios) = c("Ew", "TAN", "TAN-IN", "GMV", "GMV-c", "BS")

Turnovers = matrix(NA, nrow = 6, ncol = 6)
colnames(Turnovers) = c("10 - 120", "10 - 240", "10 - 3600", "100 - 120", "100 - 240", "100 - 3600")
rownames(Turnovers) = c("Ew", "TAN", "TAN-IN", "GMV", "GMV-c", "BS")


###############################
# Exercise 1.c (10 assets) EW #
###############################

calculate_portfolio_metrics_EW <- function(N, M, simul) {
  
  ewReturns = as.matrix(rowMeans(simul))
  # Calculate turnover
  EW_turnover = 0
  for (i in (M+1):T) {
    for (j in 1:N) {
      EW_turnover = EW_turnover + abs((1/N) - ((1/N)*simul[i,j]/ewReturns[i,1]))
    }
  }
  EW_turnover = EW_turnover/(T-M)
  EW_turnover
  
  # Calculate Sharpe ratio
  ew_sharpe <- mean(ewReturns[(M + 1):T, ]) / sd(ewReturns[(M + 1):T, ])
  
  return(list(sharpe_ratio = ew_sharpe, turnover = EW_turnover))
}

########
#N = 10
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

EW_result_10_120 <- calculate_portfolio_metrics_EW(10, 120, simulated_data)
EW_sharpe_ratio_10_120 <- EW_result_10_120$sharpe_ratio
Sharpe_Ratios[1,1] = EW_sharpe_ratio_10_120
EW_turnover_10_120 <- EW_result_10_120$turnover
Turnovers[1,1] = EW_turnover_10_120


EW_result_10_240 <- calculate_portfolio_metrics_EW(10, 240, simulated_data)
EW_sharpe_ratio_10_240 <- EW_result_10_240$sharpe_ratio
Sharpe_Ratios[1,2] = EW_sharpe_ratio_10_240
EW_turnover_10_240 <- EW_result_10_240$turnover
Turnovers[1,2] = EW_turnover_10_240

EW_result_10_3600 <- calculate_portfolio_metrics_EW(10, 3600, simulated_data)
EW_sharpe_ratio_10_3600 <- EW_result_10_3600$sharpe_ratio
Sharpe_Ratios[1,3] = EW_sharpe_ratio_10_3600
EW_turnover_10_3600 <- EW_result_10_3600$turnover
Turnovers[1,3] = EW_turnover_10_3600

########
N = 100
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

EW_result_100_120 <- calculate_portfolio_metrics_EW(100, 120, simulated_data)
EW_sharpe_ratio_100_120 <- EW_result_100_120$sharpe_ratio
Sharpe_Ratios[1,4] = EW_sharpe_ratio_100_120
EW_turnover_100_120 <- EW_result_100_120$turnover
Turnovers[1,4] = EW_turnover_100_120

EW_result_100_240 <- calculate_portfolio_metrics_EW(100, 240, simulated_data)
EW_sharpe_ratio_100_240 <- EW_result_100_240$sharpe_ratio
Sharpe_Ratios[1,5] = EW_sharpe_ratio_100_240
EW_turnover_100_240 <- EW_result_100_240$turnover
Turnovers[1,5] = EW_turnover_100_240

EW_result_100_3600 <- calculate_portfolio_metrics_EW(100, 3600, simulated_data)
EW_sharpe_ratio_100_3600 <- EW_result_100_3600$sharpe_ratio
Sharpe_Ratios[1,6] = EW_sharpe_ratio_100_3600
EW_turnover_100_3600 <- EW_result_100_3600$turnover
Turnovers[1,6] = EW_turnover_100_3600


#################################
# Exercise 1.d (10 assets) TAN  #
#################################


calculate_portfolio_metrics_TAN <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  TAN <- matrix(0, nrow = T, ncol = N)
  tanReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate TAN weights and TAN Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):(i-1), 1:N])
    mean = colMeans(simul[(i-M):(i-1),1:N])
    denominator <- t(iota) %*% solve(cov_matrix) %*% mean
    tan <- (solve(cov_matrix) %*% mean) / denominator[1, 1]
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
Sharpe_Ratios[2,1] = TAN_sharpe_ratio_10_120
TAN_turnover_10_120 <- TAN_result_10_120$turnover
Turnovers[2,1] = TAN_turnover_10_120

TAN_result_10_240 <- calculate_portfolio_metrics_TAN(10, 240, simulated_data)
TAN_sharpe_ratio_10_240 <- TAN_result_10_240$sharpe_ratio
Sharpe_Ratios[2,2] = TAN_sharpe_ratio_10_240
TAN_turnover_10_240 <- TAN_result_10_240$turnover
Turnovers[2,2] = TAN_turnover_10_240

TAN_result_10_3600 <- calculate_portfolio_metrics_TAN(10, 3600, simulated_data)
TAN_sharpe_ratio_10_3600 <- TAN_result_10_3600$sharpe_ratio
Sharpe_Ratios[2,3] = TAN_sharpe_ratio_10_3600
TAN_turnover_10_3600 <- TAN_result_10_3600$turnover
Turnovers[2,3] = TAN_turnover_10_3600

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

TAN_result_100_120 <- calculate_portfolio_metrics_TAN(100, 120, simulated_data)
TAN_sharpe_ratio_100_120 <- TAN_result_100_120$sharpe_ratio
Sharpe_Ratios[2,4] = TAN_sharpe_ratio_100_120
TAN_turnover_100_120 <- TAN_result_100_120$turnover
Turnovers[2,4] = TAN_turnover_100_120

TAN_result_100_240 <- calculate_portfolio_metrics_TAN(100, 240, simulated_data)
TAN_sharpe_ratio_100_240 <- TAN_result_100_240$sharpe_ratio
Sharpe_Ratios[2,5] = TAN_sharpe_ratio_100_240
TAN_turnover_100_240 <- TAN_result_100_240$turnover
Turnovers[2,5] = TAN_turnover_100_240

TAN_result_100_3600 <- calculate_portfolio_metrics_TAN(100, 3600, simulated_data)
TAN_sharpe_ratio_100_3600 <- TAN_result_100_3600$sharpe_ratio
Sharpe_Ratios[2,6] = TAN_sharpe_ratio_100_3600
TAN_turnover_100_3600 <- TAN_result_100_3600$turnover
Turnovers[2,6] = TAN_turnover_100_3600


############################
# Exercise 1.e (10 assets) # (TAN in sample)
############################


calculate_portfolio_metrics_TAN_in_sample <- function(N, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul) # = 20000

  tanReturns <- matrix(0, nrow = T, ncol = 1) #empty vector for tan returns
  
  #calculate covariance matrix and mean for the whole sample (M=T=20000)
  cov_matrix <- cov(simul)
  mean = colMeans(simul)
  
  # Calculate TAN weights and TAN Returns
  denominator <- t(iota) %*% solve(cov_matrix) %*% mean
  tan <- solve(cov_matrix) %*% mean / denominator[1, 1]

  for (i in 1:T) {
    tanReturns[i,1] <- t(tan) %*% simul[i, 1:N]
  }
  
  
  # Calculate Sharpe ratio
  tan_sharpe <- mean(tanReturns[1:T, ]) / sd(tanReturns[1:T, ])
  print(sqrt(t(mean) %*% solve(cov_matrix) %*% mean))
  
  return(list(sharpe_ratio = tan_sharpe))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_10_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_120_in_sample <- TAN_result_10_120_in_sample$sharpe_ratio
Sharpe_Ratios[3,1] = TAN_sharpe_ratio_10_120_in_sample

TAN_result_10_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_240_in_sample <- TAN_result_10_240_in_sample$sharpe_ratio
Sharpe_Ratios[3,2] = TAN_sharpe_ratio_10_240_in_sample

TAN_result_10_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_3600_in_sample <- TAN_result_10_3600_in_sample$sharpe_ratio
Sharpe_Ratios[3,3] = TAN_sharpe_ratio_10_3600_in_sample

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

TAN_result_100_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, simulated_data)
TAN_sharpe_ratio_100_120_in_sample <- TAN_result_100_120_in_sample$sharpe_ratio
Sharpe_Ratios[3,4] = TAN_sharpe_ratio_100_120_in_sample

TAN_result_100_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, simulated_data)
TAN_sharpe_ratio_100_240_in_sample <- TAN_result_100_240_in_sample$sharpe_ratio
Sharpe_Ratios[3,5] = TAN_sharpe_ratio_100_240_in_sample

TAN_result_100_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100,  simulated_data)
TAN_sharpe_ratio_100_3600_in_sample <- TAN_result_100_3600_in_sample$sharpe_ratio
Sharpe_Ratios[3,6] = TAN_sharpe_ratio_100_3600_in_sample



###########################################
# Exercise 1.f (10 assets)  GMV portfolio #
###########################################

calculate_portfolio_metrics_GMV <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  GMV <- matrix(0, nrow = T, ncol = N)
  gmvReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate GMV and GMV Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):(i-1), 1:N])
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
  
  return(list(return = gmvReturns, weights = GMV, sharpe_ratio = gmv_sharpe, turnover = gmv_turnover))
}

########
#N = 10#
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]
  
GMV_result_10_120 <- calculate_portfolio_metrics_GMV(10, 120, simulated_data)
GMV_sharpe_ratio_10_120 <- GMV_result_10_120$sharpe_ratio
Sharpe_Ratios[4,1] = GMV_sharpe_ratio_10_120
GMV_turnover_10_120 <- GMV_result_10_120$turnover
Turnovers[4,1] = GMV_turnover_10_120

GMV_result_10_240 <- calculate_portfolio_metrics_GMV(10, 240, simulated_data)
GMV_sharpe_ratio_10_240 <- GMV_result_10_240$sharpe_ratio
Sharpe_Ratios[4,2] = GMV_sharpe_ratio_10_240
GMV_turnover_10_240 <- GMV_result_10_240$turnover
Turnovers[4,2] = GMV_turnover_10_240

GMV_result_10_3600 <- calculate_portfolio_metrics_GMV(10, 3600, simulated_data)
GMV_sharpe_ratio_10_3600 <- GMV_result_10_3600$sharpe_ratio
Sharpe_Ratios[4,3] = GMV_sharpe_ratio_10_3600
GMV_turnover_10_3600 <- GMV_result_10_3600$turnover
Turnovers[4,3] = GMV_turnover_10_3600

##########
#N = 100 #
##########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

GMV_result_100_120 <- calculate_portfolio_metrics_GMV(100, 120, simulated_data)
GMV_sharpe_ratio_100_120 <- GMV_result_100_120$sharpe_ratio
Sharpe_Ratios[4,4] = GMV_sharpe_ratio_100_120
GMV_turnover_100_120 <- GMV_result_100_120$turnover
Turnovers[4,4] = GMV_turnover_100_120

GMV_result_100_240 <- calculate_portfolio_metrics_GMV(100, 240, simulated_data)
GMV_sharpe_ratio_100_240 <- GMV_result_100_240$sharpe_ratio
Sharpe_Ratios[4,5] = GMV_sharpe_ratio_100_240
GMV_turnover_100_240 <- GMV_result_100_240$turnover
Turnovers[4,5] = GMV_turnover_100_240

GMV_result_100_3600 <- calculate_portfolio_metrics_GMV(100, 3600, simulated_data)
GMV_sharpe_ratio_100_3600 <- GMV_result_100_3600$sharpe_ratio
Sharpe_Ratios[4,6] = GMV_sharpe_ratio_100_3600
GMV_turnover_100_3600 <- GMV_result_100_3600$turnover
Turnovers[4,6] = GMV_turnover_100_3600

#################
#GMV constrained#
#################

# Install and load the quadprog package
install.packages("quadprog")
library(quadprog)




# Function to calculate the minimum variance portfolio with no short selling
min_variance_portfolio <- function(returns, cov_matrix) {
  n <- ncol(returns)
  
  # Objective function: minimize w' * Sigma * w
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n)
  
  
  # Combine all constraints
  Amat_combined <- cbind(rep(1,n), diag(n))
  bvec_combined <- c(1, rep(0, n))
  
  # Solve the quadratic programming problem
  sol <- solve.QP(Dmat, dvec, Amat_combined, bvec = bvec_combined, meq = 1)
  
  # Extract the weights of the minimum variance portfolio
  weights <- sol$solution
  
  return(weights)
}




#function to calculate metrics for GMV constrained
calculate_portfolio_metrics_GMVC <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  GMVC <- matrix(0, nrow = T, ncol = N)
  gmvcReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate GMV and GMV Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):(i-1), 1:N])
    gmvc <- min_variance_portfolio(simul[(i - M):i, 1:N], cov_matrix)
    GMVC[i,] <- gmvc
    gmvcReturns[i,1] <- t(gmvc) %*% simul[i, 1:N]
  }
  # Calculate turnover
  gmvc_turnover <- 0
  for (i in (M + 1):T) {
    for (j in 1:N) {
      gmvc_turnover = gmvc_turnover + abs(GMVC[i, j] - GMVC[i - 1, j] * simul[i, j] / gmvcReturns[i,])
    }
  }
  gmvc_turnover <- gmvc_turnover / (T - M)
  
  # Calculate Sharpe ratio
  gmvc_sharpe <- mean(gmvcReturns[(M + 1):T, ]) / sd(gmvcReturns[(M + 1):T, ])
  
  return(list(weights = GMVC, sharpe_ratio = gmvc_sharpe, turnover = gmvc_turnover))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

GMVC_result_10_120 <- calculate_portfolio_metrics_GMVC(10, 120, simulated_data)
GMVC_sharpe_ratio_10_120 <- GMVC_result_10_120$sharpe_ratio
GMVC_turnover_10_120 <- GMVC_result_10_120$turnover

GMVC_result_10_240 <- calculate_portfolio_metrics_GMVC(10, 240, simulated_data)
GMVC_sharpe_ratio_10_240 <- GMVC_result_10_240$sharpe_ratio
GMVC_turnover_10_240 <- GMVC_result_10_240$turnover

GMVC_result_10_3600 <- calculate_portfolio_metrics_GMVC(10, 3600, simulated_data)
GMVC_sharpe_ratio_10_3600 <- GMVC_result_10_3600$sharpe_ratio
GMVC_turnover_10_3600 <- GMVC_result_10_3600$turnover

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

GMVC_result_100_120 <- calculate_portfolio_metrics_GMVC(100, 120, simulated_data)
GMVC_sharpe_ratio_100_120 <- GMVC_result_100_120$sharpe_ratio
GMVC_turnover_100_120 <- GMVC_result_100_120$turnover

GMVC_result_100_240 <- calculate_portfolio_metrics_GMVC(100, 240, simulated_data)
GMVC_sharpe_ratio_100_240 <- GMVC_result_100_240$sharpe_ratio
GMVC_turnover_100_240 <- GMVC_result_100_240$turnover

GMVC_result_100_3600 <- calculate_portfolio_metrics_GMVC(100, 3600, simulated_data)
GMVC_sharpe_ratio_100_3600 <- GMVC_result_100_3600$sharpe_ratio
GMVC_turnover_100_3600 <- GMVC_result_100_3600$turnover


#####################################
# Exercise 1.h Bayes-Stain Tangency #
#####################################

calculate_portfolio_metrics_BS <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  TAN <- matrix(0, nrow = T, ncol = N)
  tanReturns <- matrix(0, nrow = T, ncol = 1)
  
  mu_0 = mean(calculate_portfolio_metrics_GMV(N, M, simul)$return)
  
  # Calculate TAN weights and TAN Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):(i-1), 1:N])
    sample_mean = colMeans(simul[(i-M):(i-1),1:N])
    
    delta = min(1, ((N-2)/T) / ( (N+2) + t(sample_mean - mu_0 * iota) %*% solve(cov_matrix) %*% (sample_mean - mu_0 * iota) ) )
    
    mu_star = delta * mu_0 * iota + (1 - delta) * sample_mean
    
    denominator <- t(iota) %*% solve(cov_matrix) %*% mu_star
    tan <- solve(cov_matrix) %*% mu_star / denominator[1, 1]
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
  
  return(list(return = tanReturns, weights = TAN, sharpe_ratio = tan_sharpe, turnover = tan_turnover))
}


########
#N = 10#
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

BS_result_10_120 <- calculate_portfolio_metrics_BS(10, 120, simulated_data)
BS_sharpe_ratio_10_120 <- BS_result_10_120$sharpe_ratio
Sharpe_Ratios[6,1] = BS_sharpe_ratio_10_120
BS_turnover_10_120 <- BS_result_10_120$turnover
Turnovers[6,1] = BS_turnover_10_120

BS_result_10_240 <- calculate_portfolio_metrics_BS(10, 240, simulated_data)
BS_sharpe_ratio_10_240 <- BS_result_10_240$sharpe_ratio
Sharpe_Ratios[6,2] = BS_sharpe_ratio_10_240
BS_turnover_10_240 <- BS_result_10_240$turnover
Turnovers[6,2] = BS_turnover_10_240

BS_result_10_3600 <- calculate_portfolio_metrics_BS(10, 3600, simulated_data)
BS_sharpe_ratio_10_3600 <- BS_result_10_3600$sharpe_ratio
Sharpe_Ratios[6,3] = BS_sharpe_ratio_10_3600
BS_turnover_10_3600 <- BS_result_10_3600$turnover
Turnovers[6,3] = BS_turnover_10_3600

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

BS_result_100_120 <- calculate_portfolio_metrics_BS(100, 120, simulated_data)
BS_sharpe_ratio_100_120 <- BS_result_100_120$sharpe_ratio
Sharpe_Ratios[6,4] = BS_sharpe_ratio_100_120
BS_turnover_100_120 <- BS_result_100_120$turnover
Turnovers[6,4] = BS_turnover_100_120

BS_result_100_240 <- calculate_portfolio_metrics_BS(100, 240, simulated_data)
BS_sharpe_ratio_100_240 <- BS_result_100_240$sharpe_ratio
Sharpe_Ratios[6,5] = BS_sharpe_ratio_100_240
BS_turnover_100_240 <- BS_result_100_240$turnover
Turnovers[6,5] = BS_turnover_100_240

BS_result_100_3600 <- calculate_portfolio_metrics_BS(100, 3600, simulated_data)
BS_sharpe_ratio_100_3600 <- BS_result_100_3600$sharpe_ratio
Sharpe_Ratios[6,6] = BS_sharpe_ratio_100_3600
BS_turnover_100_3600 <- BS_result_100_3600$turnover
Turnovers[6,6] = BS_turnover_100_3600



######################################################################
# Exercise K : Test the difference of sharpe ratio between EW and BS #
######################################################################
library("PeerPerformance")

pvalues = matrix(NA, nrow = 3, ncol = 2)
colnames(pvalues) = c("N=10", "N=100")
rownames(pvalues) = c("M=120", "M=240", "M=3600")

########
# N=10 #
########
N <- 10

M <- 120
# assume stationariety on the returns of the two portfolios
r_ew = (EW_result_10_120$return[(M+1):20000] - mean(EW_result_10_120$return))
r_bs = (BS_result_10_120$return[(M+1):20000] - mean(BS_result_10_120$return))

ctr = list(type = 1, hac = TRUE)
out_1 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_1)
pvalues[1, 1] = out_1$pval


M <- 240
r_ew = (EW_result_10_240$return[(M+1):20000] - mean(EW_result_10_240$return))
r_bs = (BS_result_10_240$return[(M+1):20000] - mean(BS_result_10_240$return))

ctr = list(type = 1, hac = TRUE)
out_2 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_2)
pvalues[2, 1] = out_2$pval


M <- 3600
r_ew = (EW_result_10_3600$return[(M+1):20000] - mean(EW_result_10_3600$return))
r_bs = (BS_result_10_3600$return[(M+1):20000] - mean(BS_result_10_3600$return))

ctr = list(type = 1, hac = TRUE)
out_3 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_3)
pvalues[3, 1] = out_3$pval

#########
# N=100 #
#########
N <- 100

M <- 120
# assume stationariety on the returns of the two portfolios
r_ew = (EW_result_100_120$return[(M+1):20000] - mean(EW_result_100_120$return))
r_bs = (BS_result_100_120$return[(M+1):20000] - mean(BS_result_100_120$return))

ctr = list(type = 1, hac = TRUE)
out_4 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_4)
pvalues[2, 1] = out_4$pval


M <- 240
r_ew = (EW_result_100_240$return[(M+1):20000] - mean(EW_result_100_240$return))
r_bs = (BS_result_100_240$return[(M+1):20000] - mean(BS_result_100_240$return))

ctr = list(type = 1, hac = TRUE)
out_5 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_5)
pvalues[2, 2] = out_5$pval


M <- 3600
r_ew = (EW_result_100_3600$return[(M+1):20000] - mean(EW_result_100_3600$return))
r_bs = (BS_result_100_3600$return[(M+1):20000] - mean(BS_result_100_3600$return))

ctr = list(type = 1, hac = TRUE)
out_6 = sharpeTesting(r_ew, r_bs, control = ctr)
print(out_6)
pvalues[3, 2] = out_6$pval


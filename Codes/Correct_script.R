### IMPORT PACKAGES ###
set.seed(646425)
library("psych")
library("MASS")

setwd("C:/Users/510908/Documents/code/portfolio-management/Data")
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
View(ret_matrix)

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
  # Store coefficients in the table
  coefficients_table <- rbind(coefficients_table, c(paste(i), coef(regression_result)[1], coef(regression_result)[2], summary(regression_result)$coef[2, "Std. Error"]))
}
# Rename columns in the coefficients table
colnames(coefficients_table) <- c("Asset", "Intercept", "Beta", "StdErr")
# Print the coefficients table
View(coefficients_table)

#plot intercept
plot(coefficients_table$Beta, type = "l", col = "blue", xlab = "Asset", ylab = "Intercept", main = "Intercept for each asset")

#descriptive statistics for coefficients_table$Intercept and coefficients_table$Beta
coefficients_table$Beta
View(coefficients_table$StdErr)

describe(as.numeric(coefficients_table$Intercept))
describe(as.numeric(coefficients_table$Beta))

View(coefficients_table)
mean(as.numeric(coefficients_table$Intercept))
mean(as.numeric(coefficients_table$Beta))
mean(as.numeric(coefficients_table$StdErr))


#################
# Exercise 1.b #
################

sharpe_mkt = mean(mkt_factor)/sd(mkt_factor)
sharpe_mkt

''' ALTERNATIVE SIMULATION '''
#Calculate the time series of returns under the assumption that the CAPM holds.
#meaning that the returns are calculated as I

''' END ALTERNATIVE SIMULATION '''

#################
# simulation 10 #
#################

#randomly pick 10 numbers between 1 and 543
random_10 = sample(1:543, 10)
random_10
#create a matrix with 10 columns containing columns of ret_matrix with the index of random
ten_assets_matrix = ret_matrix[,random_10]
View(ten_assets_matrix)
#calculate the sample mean and variance of ten_assets_matrix (and consider these the TRUE parameters of the DGP)
ten_sample_mean = as.matrix(colMeans(ten_assets_matrix))
ten_sample_cov = cov(ten_assets_matrix)
View(ten_sample_cov)

#simulate 20000 observations from a multivariate normal distribution with mean ten_sample_mean and covariance ten_sample_cov
simulated_10 = mvrnorm(20000, ten_sample_mean, ten_sample_cov)
View(simulated_10)



##################
# simulation 100 #
##################

#randomly pick 10 numbers between 1 and 543
random_100 = sample(1:543, 100)
random_100
#create a matrix with 100 columns containing columns of ret_matrix with the index of random
hun_assets_matrix = ret_matrix[,random_100]
View(hun_assets_matrix)
#calculate the sample mean and variance of ten_assets_matrix (and consider these the TRUE parameters of the DGP)
hun_sample_mean = as.matrix(colMeans(hun_assets_matrix))
hun_sample_cov = cov(hun_assets_matrix)
View(hun_sample_cov)

#simulate 20000 observations from a multivariate normal distribution with mean ten_sample_mean and covariance ten_sample_cov
simulated_100 = mvrnorm(20000, hun_sample_mean, hun_sample_cov)
View(simulated_100)


############################
# Exercise 1.c (10 assets) #
############################
EW_returns_10 = as.matrix(rowMeans(simulated_10))
EW_returns_100 = as.matrix(rowMeans(simulated_100))

#calculate the sharpe ratio of EW Returns
EW_sharpe_10 = mean(EW_returns_10)/sd(EW_returns_10) #Ale: I changed this. before we created a new column in simulated_10 where we stored the means of the rows. this is not the best way to do i. So the EW returns are now stored in a new varlable called EW_returns_10 (same for EW_returns_100)
EW_sharpe_10

EW_sharpe_100 = mean(EW_returns_100)/sd(EW_returns_100) #Ale: I changed this. before we created a new column in simulated_10 where we stored the means of the rows. this is not the best way to do i. So the EW returns are now stored in a new varlable called EW_returns_10 (same for EW_returns_100)
EW_sharpe_100

#for the turnover of the EW portfolio, calculate turnover as the average (ovet T) sum of absolute changes in weights of assets, between the desired weight (1/10), and realised weight of the asset, being the previous weight multiplied by the asset return and divided by return of the portfolio, summed over the portfolio
#calculate the turnover of the EW portfolio
EW_turnover = 0
N = 100
T = 20000
for (i in 1:T) {
  for (j in 1:N) {
    EW_turnover = EW_turnover + abs((1/N) - ((1/N)*simulated_100[i,j]/EW_returns_100[i,1]))
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
simulated_data <- simulated_100[1:20000, 1:N]

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
  
  return(list(sharpe_ratio = tan_sharpe))
}

########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

TAN_result_10_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_120_in_sample <- TAN_result_10_120_in_sample$sharpe_ratio

TAN_result_10_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_240_in_sample <- TAN_result_10_240_in_sample$sharpe_ratio

TAN_result_10_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(10, simulated_data)
TAN_sharpe_ratio_10_3600_in_sample <- TAN_result_10_3600_in_sample$sharpe_ratio

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

TAN_result_100_120_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, simulated_data)
TAN_sharpe_ratio_100_120_in_sample <- TAN_result_100_120_in_sample$sharpe_ratio

TAN_result_100_240_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100, simulated_data)
TAN_sharpe_ratio_100_240_in_sample <- TAN_result_100_240_in_sample$sharpe_ratio

TAN_result_100_3600_in_sample <- calculate_portfolio_metrics_TAN_in_sample(100,  simulated_data)
TAN_sharpe_ratio_100_3600_in_sample <- TAN_result_100_3600_in_sample$sharpe_ratio


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
  
  return(list(return = gmvReturns, weights = GMV, sharpe_ratio = gmv_sharpe, turnover = gmv_turnover))
}

########
'N = 10'
########
mean(GMV_result_10_120$return)


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

GMV_GMV_result_100_120 <- calculate_portfolio_metrics_GMV(100, 120, simulated_data)
GMV_sharpe_ratio_100_120 <- GMV_result_100_120$sharpe_ratio
GMV_turnover_100_120 <- GMV_result_100_120$turnover

GMV_result_100_240 <- calculate_portfolio_metrics_GMV(100, 240, simulated_data)
GMV_sharpe_ratio_100_240 <- GMV_result_100_240$sharpe_ratio
GMV_turnover_100_240 <- GMV_result_100_240$turnover

GMV_result_100_3600 <- calculate_portfolio_metrics_GMV(100, 3600, simulated_data)
GMV_sharpe_ratio_100_3600 <- GMV_result_100_3600$sharpe_ratio
GMV_turnover_100_3600 <- GMV_result_100_3600$turnover

'''
################################################
# Exercise 1.h (10 assets)  Momentum Portfolio #
################################################
'''
calculate_portfolio_metrics_MOM <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  threshold <- 0.3 * N
  
  MOM <- matrix(0, nrow = T, ncol = N)
  momReturns <- matrix(0, nrow = T, ncol = 1)
  
  # Calculate MOM weights and MOM Returns
  for (i in (M + 1):T) {
    sorted_returns = order(simul[i, 1:N], decreasing = F)
    short_indices <- sorted_returns[1:threshold]
    long_indices <- sorted_returns[(N - threshold + 1):N]
    
    for( j in 1:N){
      if(j %in% short_indices){
        MOM[i,j] <- -1/threshold
      }else if(j %in% long_indices){
        MOM[i,j] <- 1/threshold
      }
    }
    
    momReturns[i,1] <- t(MOM[i,]) %*% simul[i, 1:N]
  }
  
  # Calculate turnover
  mom_turnover <- 0
  for (i in (M + 1):T) {
    for (j in 1:N) {
      mom_turnover = mom_turnover + abs(MOM[i, j] - MOM[i - 1, j] * simul[i, j] / momReturns[i,])
    }
  }
  mom_turnover <- mom_turnover / (T - M)
  
  # Calculate Sharpe ratio
  mom_sharpe <- mean(momReturns[(M + 1):T, ]) / sd(momReturns[(M + 1):T, ])
  
  return(list(returns = momReturns, weights = MOM, sharpe_ratio = mom_sharpe, turnover = mom_turnover))
}


########
'N = 10'
########

mean(MOM_result_10_120$returns)
sd(MOM_result_10_120$returns)

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

MOM_result_10_120 <- calculate_portfolio_metrics_MOM(10, 120, simulated_data)
MOM_sharpe_ratio_10_120 <- MOM_result_10_120$sharpe_ratio
MOM_turnover_10_120 <- MOM_result_10_120$turnover

MOM_result_10_240 <- calculate_portfolio_metrics_MOM(10, 240, simulated_data)
MOM_sharpe_ratio_10_240 <- MOM_result_10_240$sharpe_ratio
MOM_turnover_10_240 <- MOM_result_10_240$turnover

MOM_result_10_3600 <- calculate_portfolio_metrics_MOM(10, 3600, simulated_data)
MOM_sharpe_ratio_10_3600 <- MOM_result_10_3600$sharpe_ratio
MOM_turnover_10_3600 <- MOM_result_10_3600$turnover

########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

MOM_result_100_120 <- calculate_portfolio_metrics_MOM(100, 120, simulated_data)
MOM_sharpe_ratio_100_120 <- MOM_result_100_120$sharpe_ratio
MOM_turnover_100_120 <- MOM_result_100_120$turnover

MOM_result_100_240 <- calculate_portfolio_metrics_MOM(100, 240, simulated_data)
MOM_sharpe_ratio_100_240 <- MOM_result_100_240$sharpe_ratio
MOM_turnover_100_240 <- MOM_result_100_240$turnover

MOM_result_100_3600 <- calculate_portfolio_metrics_MOM(100, 3600, simulated_data)
MOM_sharpe_ratio_100_3600 <- MOM_result_100_3600$sharpe_ratio
MOM_turnover_100_3600 <- MOM_result_100_3600$turnover

'''
#####################################
# Exercise 1.h Bayes-Stain Tangency #
#####################################
'''

calculate_portfolio_metrics_BS <- function(N, M, simul) {
  iota <- matrix(1, nrow = N, ncol = 1)
  T <- nrow(simul)
  
  TAN <- matrix(0, nrow = T, ncol = N)
  tanReturns <- matrix(0, nrow = T, ncol = 1)
  
  mu_0 = mean(calculate_portfolio_metrics_GMV(N, M, simul)$return)
  
  # Calculate TAN weights and TAN Returns
  for (i in (M + 1):T) {
    cov_matrix <- cov(simul[(i - M):i, 1:N])
    sample_mean = colMeans(simul[(i-M):i,1:N])
    
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
  
  return(list(weights = TAN, sharpe_ratio = tan_sharpe, turnover = tan_turnover))
}


########
'N = 10'
########

N <- 10
simulated_data <- simulated_10[1:20000, 1:N]

BS_result_10_120 <- calculate_portfolio_metrics_BS(10, 120, simulated_data)
BS_sharpe_ratio_10_120 <- BS_result_10_120$sharpe_ratio
BS_turnover_10_120 <- BS_result_10_120$turnover

BS_result_10_240 <- calculate_portfolio_metrics_BS(10, 240, simulated_data)
BS_sharpe_ratio_10_240 <- BS_result_10_240$sharpe_ratio
BS_turnover_10_240 <- BS_result_10_240$turnover

BS_result_10_3600 <- calculate_portfolio_metrics_BS(10, 3600, simulated_data)
BS_sharpe_ratio_10_3600 <- BS_result_10_3600$sharpe_ratio
BS_turnover_10_3600 <- BS_result_10_3600$turnover


########
'N = 100'
########

N <- 100
simulated_data <- simulated_100[1:20000, 1:N]

BS_result_100_120 <- calculate_portfolio_metrics_BS(100, 120, simulated_data)
BS_sharpe_ratio_100_120 <- BS_result_100_120$sharpe_ratio
BS_turnover_100_120 <- BS_result_100_120$turnover

BS_result_100_240 <- calculate_portfolio_metrics_BS(100, 240, simulated_data)
BS_sharpe_ratio_100_240 <- BS_result_100_240$sharpe_ratio
BS_turnover_100_240 <- BS_result_100_240$turnover

BS_result_100_3600 <- calculate_portfolio_metrics_BS(100, 3600, simulated_data)
BS_sharpe_ratio_100_3600 <- BS_result_100_3600$sharpe_ratio
BS_turnover_100_3600 <- BS_result_100_3600$turnover

# CS5565 Assignment #2
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 5.4 Applied Problem 9

# Load the Boston data set from the MASS library
library(MASS)

View(Boston)
?Boston

# a) Provide an estimate for the population mean of medv (Median home value in $1000s), denoted as ˆµ
mean(Boston$medv)

# b) Provide an estimate of the standard error of ˆµ.
# The standard error of the sample mean is computed by dividing the sample standard deviation by the square root of the number of observations.
s.se = sd(Boston$medv)/ sqrt(dim(Boston)[1])
s.se

# The standard error of 0.4088611 signifies that the estimate for the population mean should differ by about $408.86 from the actual population mean

# c) Estimate the standard error of ˆµ using the bootstrap

set.seed(312)

# Create function for finding population mean estimate to be used in bootstrap
boot.fn = function(data, index){
  return(mean(data$medv[index]))
}
# Returns result of bootstrap analysis for R= 10,000 bootstrap estimates
boot(Boston, boot.fn, 10000)

# The bootstrap estimate of the standard error is 0.406628, which is slightly lower than the estimate computed from part b).

# d) Based on your bootstrap estimate from (c), provide a 95 % confidence interval for the mean of medv. Compare it to the results obtained using t.test(Boston$medv)
# Approximate a 95% confidence interval using the formula [μ^−2SE(μ^),μ^+2SE(μ^)]
22.53281 - 2 * 0.406628
22.53281 + 2 * 0.406628

t.test(Boston$medv)
# The sample t-test yields a slightly narrower confidence interval than the bootstrap estimate.

# e) Provide an estimate, ˆµmed, for the median value of medv in the population
median(Boston$medv)

# f) Estimate the standard error of the median using the bootstrap.
set.seed(312)

# Create function for finding population median estimate to be used in bootstrap
boot.fn2 = function(data, index){
  return(median(data$medv[index]))
}

# Returns result of bootstrap analysis for R= 10,000 bootstrap estimates
boot(Boston, boot.fn2, 10000)

# Find 95% confidence interval based on bootstrap estimate of standard error
22.1 - 2 * 0.3758595
22.1 + 2 * 0.3758595

# The bootstrap standard error estimate of 0.3758595 gives a 95% confidence interval of [21.34828, 22.85172]. There is a 
# 95% chance this confidence interval contains the true population median.

# g) Based on this data set, provide an estimate for the tenth percentile of medv in Boston census tracts. Call this quantity ˆµ0.1
quantile(Boston$medv, c(0.1))

# h) Use the bootstrap to estimate the standard error of ˆµ0.1.
# f) Estimate the standard error of the median using the bootstrap.
set.seed(312)

# Create function for finding population 10th percentile estimate to be used in bootstrap
boot.fn3 = function(data, index){
  return(quantile(data$medv[index], 0.1))
}

# Returns result of bootstrap analysis for R= 10,000 bootstrap estimates
boot(Boston, boot.fn3, 10000)

# Find 95% confidence interval based on bootstrap estimate of standard error
12.75 - 2 * 0.4982062
12.75 + 2 * 0.4982062

# The bootstrap standard error estimate of 0.4982062 gives a 95% confidence interval of [11.75359, 13.74641]. There is a 
# 95% chance this confidence interval contains the true 10th quantile for the population.

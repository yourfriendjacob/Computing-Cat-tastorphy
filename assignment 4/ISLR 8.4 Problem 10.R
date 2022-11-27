# CS5565 Assignment #4
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# Contains gbm package to perform boosting
library(gbm)

# Contains glmnet to perform lasso regression
library(glmnet)

# Contains randomForest() to perform bagging
library(randomForest)

# Required package for %>%
library(dplyr)

# Contains Hitter dataset
library(ISLR)

# ISLR 8.4 Applied Problem 10

# a) Remove observations with unknown salaries, then log transform the salaries

Hitters_NAomit = Hitters[!(is.na(Hitters$Salary)), ]

# Extreme values will skew the predictions made by regression trees

# Log transform salary to reduce skew and create a new variable for the transformation
Hitters_NAomit$logSalary = log(Hitters_NAomit$Salary)

# b) Create a training set with the first 200 observations and a test set with the remaining observations
train = Hitters_NAomit[1:200,]

# The test set is comprised of rows 201 through the last row in the dataset
test = Hitters_NAomit[201:nrow(Hitters_NAomit),]

# c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter.

# Generates a sequence of lambda values from 10^-6 to 1, sampled in intervals of 0.1
lambdaRange <- 10^seq(-6, 0, 0.1)

# The shrinkage parameter must be a small number close to 0, hence the small range of lambda

set.seed(1)

# Create a data frame for training MSE
train_MSE = c()

# Use a for loop to perform boosting for each shrinkage value, then use boosting model to predict train MSE
for (i in 1:length(lambdaRange)) {
  boost_TEMP = gbm(logSalary ~ . - Salary, 
                    data = train, 
                    distribution = "gaussian", 
                    n.trees = 1000, 
                    interaction.depth = 2, 
                    shrinkage = lambdaRange[i])
  
  train_MSE[i] = mean((predict(boost_TEMP, train, n.trees = 1000) - train$logSalary)^2)
}

# Produce a plot with different shrinkage values on the x-axis and corresponding training set MSE on y-axis
data.frame(lambda = lambdaRange, train_MSE) %>%
  ggplot(aes(x = lambda, y = train_MSE)) + 
  geom_point(size = 2, col = "deepskyblue3") + 
  geom_line(col = "grey55") + 
  scale_x_continuous(trans = 'log10', breaks = 10^seq(-6, 0), labels = 10^seq(-6, 0), minor_breaks = NULL) + 
  labs(x = "Lambda (Shrinkage)", 
       y = "Training MSE")

# d) Produce a plot with different shrinkage values on x-axis and corresponding test set MSE on y-axis

# Create a data frame for test MSE
test_MSE = c()

# Use a for loop to use boosting model to predict tests MSE
for (i in 1:length(lambdaRange)) {
  boost_TEMP <- gbm(logSalary ~ . - Salary, 
                    data = train, 
                    distribution = "gaussian", 
                    n.trees = 1000, 
                    interaction.depth = 2, 
                    shrinkage = lambdaRange[i])
  
  test_MSE[i] = mean((predict(boost_TEMP, test, n.trees = 1000) - test$logSalary)^2)
}

data.frame(lambda = lambdaRange, test_MSE) %>%
  ggplot(aes(x = lambda, y = test_MSE)) + 
  geom_point(size = 2, col = "deepskyblue3") + 
  geom_line(col = "grey55") + 
  scale_x_continuous(trans = 'log10', breaks = 10^seq(-6, 0), labels = 10^seq(-6, 0), minor_breaks = NULL) + 
  labs(x = "Lambda (Shrinkage)", 
       y = "Test MSE")

# e) Compare the test MSE of boosting to the test MSE resulting from applying regression approaches
# Test MSE for boosting is minimum MSE generated from boosting model
min(test_MSE)

# The minimum test MSE from boosting is 0.2643016

# Fit a linear model using least squares regression on the test set
Lin.fit = lm(formula = logSalary ~ .-Salary, data = test)
Lin.pred = predict(Lin.fit, test)

Lin.MSE = mean(mean((Lin.pred-test$logSalary)^2))

# The minimum test MSE from least squares regression is 0.2851553

# Fit a lasso regression on the test set, with lambda chosen by 5-fold cross-validation.
set.seed(1)

# Create matrix for x to be used in cross-validation for lasso 
x = dummyVars(logSalary ~ .-Salary, data = test, fullRank = F) %>%
  predict(newdata = test) %>%
  as.matrix()

CV.Lasso = cv.glmnet(x= x, y= test$logSalary, alpha = 1, lambda = seq(0, 0.1, 0.001), standardize = TRUE, nfolds= 5)
CV.Lasso$lambda.min

# Fit a lasso model on test set
Lasso.mod = glmnet(x, y=test$logSalary, alpha = 1, lambda = seq(0, 0.1, 0.001))

# Predict MSE using the lambda obtained from 5-fold cross validation on the test data
Lasso.pred = predict(Lasso.mod, s= CV.Lasso$lambda.min, newx = x)
Lasso.MSE = mean(mean((Lasso.pred-test$logSalary)^2))

# The minimum test MSE from the lasso is 0.4073894

# The method with the lowest MSE is boosting, though it's not a significant decrease from least squares regression

# f) Which variables are the most important predictors in the boosted model?
summary(boost_TEMP)

# Based on the relative influence ranking, CAtBat and Assists are the most influential predictors in the boosted model.

# g) Apply bagging to the training set and find the test set MSE for this approach
set.seed(1)

# Generate bagging model
Bag.model = randomForest(logSalary ~ ., data = train, mtry = 19, ntree = 500) 
Bag.MSE = mean((predict(Bag.model, newdata = test)-test$logSalary)^2)

# The Bag MSE is 0.0002589154
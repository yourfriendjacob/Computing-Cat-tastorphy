# CS5565 Assignment #3
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 6.6 Applied Problem 9

# Load college dataset into college object
college = read.csv("College.csv", header = TRUE)
View(college)

# Remove first column
college = college[, -1]

# a) Split the data set into a training set and a test set.

# Set a set so that results are reproducible
set.seed(3)

# Randomly sample a subset of numbers between 1 and n to use as the indices for the training observations.
train_index = sample(1: nrow(college), round(nrow(college) * 0.7))

# Create a data frame for training set from the randomly sampled observations. Drop first 2 columns because they are factors
train = college[train_index,]

# The observations not in the training set will be stored in the test set data frame
test = college[-train_index,]


# b) Fit a linear model using least squares on the training set, and
# report the test error obtained.

# Fits a multiple linear regression model that includes all the predictors from the College data set
Lin.fit = lm(formula = Apps ~ ., data = train)


# Generate a set of predicted values based on the linear fit to be used in the test error calculation using the test data
Lin.pred = predict(Lin.fit, test)

# Calculate the MSE as a measure of the test error
# MSE = Average squared difference between predicted value based on model and actual test value 
Lin.MSE = mean(mean((Lin.pred-test$Apps)^2))

# The test error (MSE) is approximately 1413287

# c) Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained. 
# Ridge regression passes in a matrix for x and a vector for y
x = model.matrix(Apps ~ ., train)
test.x = model.matrix (Apps ~ ., test)
y = train$Apps

set.seed(3)

# Perform 5-fold cross-validation to determine best lambda
CV.Ridge <- cv.glmnet(x=x, y=y, nfolds= 5)

# Select the lambda with the smallest mean cross-validated error
CV.Ridge$lambda.min

# alpha = 0 for ridge regression, alpha = 1 for lasso
Ridge.mod = glmnet(x, y, alpha = 0, lambda = CV.Ridge$lambda.min)

# Predict MSE using the lambda obtained from 5-fold cross validation on the test data
Ridge.pred = predict(Ridge.mod, s= CV.Ridge$lambda.min, newx = test.x)
Ridge.MSE = mean(mean((Ridge.pred-test$Apps)^2))

# The Ridge MSE is approximately 1474430

# d) Fit a lasso model on the training set, with λ chosen by cross-validation. Report the test error obtained, 
# along with the number of non-zero coefficient estimates.

# Lambda is the same one chosen as the lambda from c)
set.seed(3)
CV.Lasso = cv.glmnet(x=x, y=y, nfolds= 5)
CV.Lasso$lambda.min

# alpha = 0 for ridge regression, alpha = 1 for lasso
Lasso.mod = glmnet(x, y, alpha = 1, lambda = CV.Lasso$lambda.min)

# Predict MSE using the lambda obtained from 5-fold cross validation on the test data
Lasso.pred = predict(Lasso.mod, s= CV.Lasso$lambda.min, newx = test.x)
Lasso.MSE = mean(mean((Lasso.pred-test$Apps)^2))

# The Lasso MSE is approximately 1441435

# Predict lasso coefficients
Lasso.coeff = predict(Lasso.mod, type = "coefficients", s = CV.Lasso$lambda.min)
round (Lasso.coeff, 3)

# There are 17 nonzero coefficients

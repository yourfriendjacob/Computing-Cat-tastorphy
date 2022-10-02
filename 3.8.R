<<<<<<< HEAD
# CS5565 Assignment #1 
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 3.7 Applied Problem 8

# Load Auto dataset into Auto object and remove all NA values
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)

# a) Perform a simple linear regression with horsepower as predictor and mpg as the response
Auto.fit = lm(mpg ~ horsepower, data = Auto)
summary(Auto.fit)

# The relationship between the predictor and response is written as mpg = -0.1578 horsepower + 39.9359
# There is a negative relationship between the predictor and response since the coefficient in front of horsepower
# is negative. There is a moderately strong relationship between horsepower and mpg since the R^2 statistic is 0.6059.

# Using the linear fit, predict mpg associated with a horsepower of 98
predict(Auto.fit, data.frame(horsepower = 98))

# Associated 95 % confidence and prediction intervals given horsepower of 98
predict(Auto.fit, data.frame(horsepower = 98), interval = "confidence")
predict(Auto.fit, data.frame(horsepower = 98), interval = "prediction")

# b) Use the response variable mpg and predictor horsepower to generate a scatterplot
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")

# Generate the least squares regression line with a line width of 3 and red color for visibility
abline(Auto.fit, lwd= 3, col = "red")

# c) Produce diagnostic plots of the least squares regression fit
par(mfrow = c(2, 2))
plot(Auto.fit)

# Findings: The presence of a U-shaped pattern in the residuals plot indicates there is not a linear relationship between
=======
# CS5565 Assignment #1 
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 3.7 Applied Problem 8

# Load Auto dataset into Auto object and remove all NA values
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)

# a) Perform a simple linear regression with horsepower as predictor and mpg as the response
Auto.fit = lm(mpg ~ horsepower, data = Auto)
summary(Auto.fit)

# The relationship between the predictor and response is written as mpg = -0.1578 horsepower + 39.9359
# There is a negative relationship between the predictor and response since the coefficient in front of horsepower
# is negative. There is a moderately strong relationship between horsepower and mpg since the R^2 statistic is 0.6059.

# Using the linear fit, predict mpg associated with a horsepower of 98
predict(Auto.fit, data.frame(horsepower = 98))

# Associated 95 % confidence and prediction intervals given horsepower of 98
predict(Auto.fit, data.frame(horsepower = 98), interval = "confidence")
predict(Auto.fit, data.frame(horsepower = 98), interval = "prediction")

# b) Use the response variable mpg and predictor horsepower to generate a scatterplot
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")

# Generate the least squares regression line with a line width of 3 and red color for visibility
abline(Auto.fit, lwd= 3, col = "red")

# c) Produce diagnostic plots of the least squares regression fit
par(mfrow = c(2, 2))
plot(Auto.fit)

# Findings: The presence of a U-shaped pattern in the residuals plot indicates there is not a linear relationship between
>>>>>>> d9da6a7bfe3938911b24cb35dd6d26ad3d93eec6
# the predictor and response variables. 
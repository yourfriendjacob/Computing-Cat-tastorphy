#Jacob Sauther
#CS5565
#assignment 1, question 8 3.7
#Fall 2022

auto_data = read.csv("C:/Users/Jacob/Documents/GitHub/UMKC/umkc/5565/assignment 1/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
View(auto_data)
dim(auto_data)
auto_data = na.omit(auto_data)

auto.lin.fit = lm(auto_data$mpg ~ auto_data$horsepower)
summary(auto.lin.fit)
# ~Y = 39.935861 - 0.157845X

#there appears to be a relationship between the predictor and response.
#the relationship seems fairly strong since it has a low p value (p-value: < 2.2e-16)
# and a high R^2 value: 0.6059.

predict(auto.lin.fit, data.frame(horsepower = 98), interval = 'confidence')
predict(auto.lin.fit, data.frame(horsepower = 98), interval = 'prediction')
# prediction: 24.151388 min: 14.4938885 max: 33.80889
# confidence: 24.151388 min: 23.660958 max: 24.641817

plot(auto_data$horsepower, auto_data$mpg, xlab = "horsepower", ylab = "miles per gallon")
abline(auto.lin.fit, lwd = 3, col = 'red')
#the largest issues I am seeing here is that it looks like from the data that the function f
# might actually be a quadratic function. Despite the function being pretty close to the data
# I think it may be possible for the function to be even closer. The residuals appear to have
# a u shape which could mean there is non-linearity with this data.


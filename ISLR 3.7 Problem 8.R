# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001

# read csv file and clear data
Auto = read.csv("Auto.csv", na.strings = "?")
Auto = na.omit(Auto)

# use lm() to get the formula
auto.fit = lm(mpg~horsepower, Auto)
summary(auto.fit)
print("mpg = -0.157845 * horsepower + 39.935861", quote = FALSE)
print("since the R-squared is 0.6, there is a strong negative relationship between horsepower and mpg")
print("which means, within 1 increase in horsepower, there is 0.157845 decrease in mpg")

# make a prediction
predict(auto.fit, data.frame(horsepower = 98), interval = "confidence")
predict(auto.fit, data.frame(horsepower = 98), interval = "prediction")

# visualization
par(mfrow=c(1,1))
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(auto.fit, lwd = 3, col = "red")

# diagnostic plots of the linear regression fit
par(mfrow = c(2, 2))
plot(auto.fit)

print("there is a clear U curve in residuals vs fitted, this tells us that is not a good linear fit")


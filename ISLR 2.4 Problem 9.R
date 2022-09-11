# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001

# read csv file
Auto = read.csv("Auto.csv", na.strings = "?")

# omit the data with missing value
Auto = na.omit(Auto)
summary(complete.cases(Auto))

# check variables
sapply(Auto, class)
# Name is qualitative  and all the rest are quantitative

# range of Auto
apply(Auto[, 1:7], MARGIN = 2, FUN = "range")
cat("The ranges for each quantitative predictor is:\nmpg = 37.6\ncylinders = 5\ndisplacement = 387\nhorsepower = 184\nweight = 3527\nacceleration = 16.8\nyear = 12")

# mean and standard deviation
apply(Auto[, 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")

# remove 10th to 85th observation and find range, mean, sd
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")

# visualization
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")
cat("By observing the plot, I find out the lighter the vehicle, the higher mpg\nOn otherhand, the newer the model, the higher mpg")

# replace the values in that column with their meanings and convert it to a factor column
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)

# visualization
par(mfrow = c(1, 1))
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")

cat("By observing the plot, I find out the vehical make in Japanese tend to have higher average mpg")


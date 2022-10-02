# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001
# Feng Zheng

# load the library to get data set
library(MASS)

# information of Boston data set
?Boston
print("there is 506 rows and 14 columns in Boston data set")

pairs(Boston)
# some pairwise scatter plots
pairs(Boston)

par(mfrow = c(2, 2))
plot(Boston$dis, Boston$nox, ylab = "nitrogen oxides concentration (10 million)", xlab = "distances to five Boston employment centres")
plot(Boston$dis, Boston$zn, ylab = "proportion of residential land zoned for lots over 25,000 sq.ft.", xlab = "distances to five Boston employment centres")
plot(Boston$dis, Boston$black, ylab = "proportion of blacks", xlab = "distances to five Boston employment centres")
plot(Boston$crim, Boston$age, ylab = "proportion of owner-occupied units built prior to 1940", xlab = "capita crime rate")
print("I find the the farther from the five Boston employment centres, the lower nitrogen oxides concentration")
print("I find that the higher proportion of owner-occupied units built prior to 1940 the higher capita crime rate. ")


par(mfrow = c(2, 2))
hist(Boston$crim, xlab = "Per capita crime rate", main = "Boston crime rates")
hist(Boston$tax, xlab = "Tax rate per 10000 USD", main = "Boston tax rates")
hist(Boston$ptratio, xlab = "Pupil-teacher ratio", main = "Boston pupil-teacher ratios")
summary(Boston[, c(1, 10, 11)])
print("There was some area in Boston has high capita crime rate. There is also a gap on the tax rates. Lastly, pupil-teacher ratio was low")

# how many tracks bound with Charles river?
cat("In this data set,", sum(Boston$chas),"tracks bound with Charles river")

# what is the median of pupil-teacher ratio among towns in this data set?
cat("The median of median pupil-teacher ratio among towns is", median(Boston$ptratio))

# which suburb of Boston has the lowest median value of owner-occupied homes?
Boston[Boston$medv == min(Boston$medv), ]
print("The value of both suburb of Boston has the lowest median value of owner-occupied homes were similar")

# in this data set, how many of the suburbs average more than seven rooms per dwelling?
# more than eight rooms per dwelling?
sum(Boston$rm > 7)
sum(Boston$rm > 8)

# comment on the suburbs that average more than eight rooms per dwelling
Boston[Boston$rm > 8, ]
summary(Boston[Boston$rm > 8, ])
print("the suburbs that average more than eight rooms per dwelling has similar proporation of blacks")


# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001

#--------------------------------------------------------------------------------------------------------

# Salam - Chapter 2.4 Question 8
# Read the file
college= read.csv("college.csv", header = TRUE)

#run the view function on the dataset
#created a new column to duplicate the first column
rownames(college)= college[, 1]
View(college)
#deleted column1
college = college[, -1]
View(college)
head(college)

#use the summary function
summary(college)

#change the dataset to numeric then run the pairs function
college[,1] = as.numeric(factor(college[,1]))
pairs(college[,1:10])

#Use the plot() function to produce side-by-side boxplots of Outstate versus Private
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

#create the Elite column to show the top10 over 50%
Elite = rep("No", nrow(college))
Elite[college$Top10per>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

#use the summary function to show the Elite column
summary(college$Elite)

#Now use the plot() function to produce side-by-side boxplots of Outstate versus Elite
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

#show plots for the number of applicants per the different types of schools
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "2"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "1"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

#show plots for the contributions of alumni to donations to the various schools
#donation percentages are higher for private and elite alumni than the regular schools, which indicates that the alumni from Elite 
#and private schools end up donating more after graduation
par(mfrow = c(2,2))
hist(college$perc.alumni, xlab= "Percent of Alumni that donate", main= "All Alumni that donate")
hist(college$perc.alumni[college$Private =="2"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Private schools")
hist(college$perc.alumni[college$Private =="1"], xlab= "Percent of Alumni that donate", main= "Alumni donating in non-Private schools")
hist(college$perc.alumni[college$Elite =="Yes"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Elite schools")


#show plots and histagrams for the student/faculty ratio
#There are less students per faculty in the Elite and Private schools
plot(college$Private, college$S.F.Ratio, xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="1"], college$S.F.Ratio[college$Private =="1"], xlab = "non-Private Schools", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="2"], college$S.F.Ratio[college$Private =="2"], xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Elite =="Yes"], college$S.F.Ratio[college$Elite =="Yes"], xlab = "Elite", ylab = "S.F.Ratio")


par(mfrow = c(2,2))
hist(college$S.F.Ratio, xlab= "Student/faculty ratio", main= " Overall Student/faculty ratio")
hist(college$S.F.Ratio[college$Private =="2"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Private schools")
hist(college$S.F.Ratio[college$Private =="1"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in non-Private schools")
hist(college$S.F.Ratio[college$Elite =="Yes"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Elite schools")

#--------------------------------------------------------------------------------------------------------

# Jacob - Chapter 2.4 Question 9
#load auto data
auto_data = read.csv("C:/Users/Jacob/Documents/GitHub/UMKC/umkc/5565/assignment 1/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
View(auto_data)
dim(auto_data)
auto_data = na.omit(auto_data)

#--- quantitative predictors
# * mpg
# * cylinders
# * displacement
# * weight
# * acceleration
# * year
# * horsepower

#--- qualitative predictors
# * name
# * origin


mpg_range = range(auto_data$mpg)          # range 9 - 46.6
cyl_range = range(auto_data$cylinders)    # range 3 - 8
dis_range = range(auto_data$displacement) # range 68 - 455
wgt_range = range(auto_data$weight)       # range 1613 - 5140
acc_range = range(auto_data$acceleration) # range 8 - 24.8
yar_range = range(auto_data$year)         # range 70 - 82
hrp_range = range(auto_data$horsepower)   # range 46 - 230

mpg_mean = mean(auto_data$mpg)            # mean ~23.446
cyl_mean = mean(auto_data$cylinders)      # mean ~5.419
dis_mean = mean(auto_data$displacement)   # mean ~193.412
wgt_mean = mean(auto_data$weight)         # mean ~2977.584
acc_mean = mean(auto_data$acceleration)   # mean ~15.541
yar_mean = mean(auto_data$year)           # mean ~75.980
hrp_mean = mean(auto_data$horsepower)     # mean ~104.469

mpg_sd = sd(auto_data$mpg)                # sd ~7.805
cly_sd = sd(auto_data$cylinders)          # sd ~1.706
dis_sd = sd(auto_data$displacement)       # sd ~104.644
wgt_sd = sd(auto_data$weight)             # sd ~849.403
acc_sd = sd(auto_data$acceleration)       # sd ~2.759
yar_sd = sd(auto_data$year)               # sd ~3.684
hrp_sd = sd(auto_data$horsepower)         # sd ~38.450


#range
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "range")
# mpg          11.0 - 46.6
# cylinders    3 - 8
# displacement 68 - 455
# horsepower   46 - 230
# weight       1649 - 4997
# acceleration 8.5 - 24.8
# year         70 - 82

#mean
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
# mpg          24.404
# cylinders    5.373
# displacement 187.241
# horsepower   100.722
# weight       2935.972
# acceleration 15.727
# year         77.146

#standard deviation
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "sd")
# mpg          7.867
# cylinders    1.654
# displacement 99.678
# horsepower   35.709
# weight       811.300
# acceleration 2.694
# year         3.106

par(mfrow = c(2, 4))
plot(auto_data$mpg, auto_data$weight)
plot(auto_data$mpg, auto_data$cylinders)
plot(auto_data$acceleration, auto_data$horsepower)
plot(auto_data$acceleration, auto_data$cylinders)
plot(auto_data$mpg, auto_data$year)
plot(auto_data$horsepower, auto_data$year)
plot(auto_data$horsepower, auto_data$displacement)
plot(auto_data$mpg, auto_data$horsepower)
# it looks like mpg improves as a car gets lighter. The number of cylinders also seem to make an impact too.
# I see similar results with mpg on horsepower.
# it is kinda interesting to see that more horsepower appears to indicate less acceleration
# better acceleration appears to co-related with less cylinders
# higher displacement appears to mean higher horsepower

#
pairs(auto_data)

#Looking at the pairs for the data I think cylinders, displacement, horsepower, and weight. Could be used to predict the mpg of a car.
#It appears between all the predictors these seem to have a high impact on the mpg. Acceleration may also help, but judging from its relationship with
# horsepower it would seem that you would not need to have both to make a prediction on mpg.

#--------------------------------------------------------------------------------------------------------

# Feng Zheng - Chapter 2.4 Question 10

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

#--------------------------------------------------------------------------------------------------------

# Michelle - Chapter 3.7 Question 8

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
# the predictor and response variables. 

#--------------------------------------------------------------------------------------------------------

# Sehtab - Chapter 3.7 Question 9

### Part 1 ###
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
head(Auto)

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

### Part  2 ###
cor(Auto[,-c(8, 9)])

### Part 3 ###
mpg.fit = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit)

contrasts(Auto$origin)

library(car)
vif(lm(mpg ~ . - origin - name, data = Auto))

### Part 4 ###
par(mfrow = c(2, 2))
plot(mpg.fit)

### Part 5 ###
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - displacement:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:year)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . + cylinders:origin - displacement:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:weight)
summary(mpg.fit.reduced.interactions)

summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))

### Part 6 ###
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)

summary(lm(mpg ~ acceleration, data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))

summary(lm(mpg ~ log(acceleration), data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))

displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)

displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)

anova(displacement.linear, displacement.quadratic)

displacement.quintic = lm(mpg ~ poly(displacement, 5), data = Auto)
summary(displacement.quintic)

anova(displacement.quadratic, displacement.quintic)

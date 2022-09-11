# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001

# read csv file and clear data
Auto = read.csv("Auto.csv", na.strings = "?")
Auto = na.omit(Auto)

# replace the values in that column with their meanings and convert it to a factor column
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)

# scatter plot of each column
pairs(Auto[,1:7])

# correlation between each other
cor(Auto[,1:7])

# perform a multiple linear regression with mpg as the response
mpg.fit = lm(mpg ~ . -name, Auto)
summary(mpg.fit)
print("within the low p value of displacement, weight, year, and orgin. They have strong relationship with mpg")
print("the strong relationship of year tells me that the newer model, higher mpg")

# diagnostic plots of the linear regression fit
par(mfrow = c(2, 2))
plot(mpg.fit)
print("there is small u in residual vs fitted tells me that is not an linear data")
print("the residuals vs leverage plot tells me that there are some outliers")

# fit a linear regression model using all possible
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)
# adjusted R-squared = 0.8866

# horsepower and originEuropean has the highest intercept p-value, so we are going to remove it
mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8872, so the removing improved model

# removing the next intercept highest p-value, displacement and horsepower
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - displacement:horsepower)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8875, so the removing improved model

# removing the next intercept highest p-value, weight and acceleration
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:acceleration)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8877, so the removing improved model

# removing the next intercept highest p-value, weight and year
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:year)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.888, so the removing improved model

# removing the next intercept highest p-value, cylinders and horsepower
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:horsepower)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8882, so the removing improved model

# removing the next intercept highest p-value, cylinders and origin
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared decreased to 0.8879, so the removing does not improved model

# undo the step and removing the second highest intercept p=value, displacement and acceleration
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . + cylinders:origin - displacement:acceleration)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8884, so the removing improved model

# removing the next intercept highest p-value, cylinders and origin
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared decreased to 0.8882, so the removing does not improved model

# continue with removing more interaction terms to see if our model fit improves again, cylinders and displacement
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8883, so the removing improved model

# removing the next intercept highest p-value, cylinders and weight
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:weight)
summary(mpg.fit.reduced.interactions)
#the adjusted R-squared increased to 0.8886, so the removing improved model

# the interaction left was seems does not improved adjusted R-squared, grouped by level of statistical significance
# p-value less than 0.001:        cylinders:acceleration, displacement:originJapanese, 
#                                 acceleration:originEuropean, acceleration:originJapanese
# p-value between 0.001 and 0.01: weight:originJapanese
# p-value between 0.01 and 0.05:  displacement:year, horsepower:year, acceleration:year, 
#                                 year:originEuropean, year:originJapanese
# p-value between 0.05 and 0.1:   horsepower:acceleration
# p-value greater than 0.1:       cylinders:year, displacement:originEuropean, 
#                                 horsepower:weight, weight:originEuropean

# interaction effect between the number of engine cylinders and engine displacement
summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))
# the p-value of cylinders:displacement was essentially zero, so that is a statistically significant interaction

# to get a sense which variable to get transform, check each plot
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)

# transform of acceleration
acceleration.liner = lm(mpg ~ acceleration, data = Auto)
summary(acceleration.liner)
par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))
# the plot are very scattered

# try log
acceleration.log = lm(mpg ~ log(acceleration), data = Auto)
summary(acceleration.log)
par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))
# the adjusted R-squared only increased a little, so it did not do very much with log

# transform of displacement
displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(lm(mpg ~ displacement, data = Auto))

displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)

# anova() a statistical test for estimating how a quantitative dependent variable changes 
# according to the levels of one or more categorical independent variables
anova(displacement.linear, displacement.quadratic)
# as we can see, the p-value was essentially zero,so that is statistically significant.
# adjusted R-squared increase from 0.6473 to 0.6872, 
# the result shows in anova() indicate that includes the quadratic term is a better fit model


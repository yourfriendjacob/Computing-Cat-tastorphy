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

# 1.Yes, there is relation between predictors and the response.
# 2. Displacement, weight, year, originEuropean, originJapanese have statistically signifcant relationship
#3. year has strong statistical significance
contrasts(Auto$origin)

library(car)
vif(lm(mpg ~ . - origin - name, data = Auto))

### Part 4 ###
par(mfrow = c(2, 2))
plot(mpg.fit)
# Residual Vs Leverage plot has some outliers.High leverage has low residuals.

### Part 5 ###
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)

# acceleration:originEuropean has strong statistical significance.acceleration:year,  acceleration:originJapanese, year:originEuropean,
# year:originJapanese has also statistical significance.

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

# acceleration is statistically significant

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

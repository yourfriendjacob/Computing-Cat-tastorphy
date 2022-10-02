########################### Applied Exercise 1 ##########################

### Part 1 ###
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)

auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)

predict(auto.lin.fit, data.frame(horsepower = 98), interval = "confidence")

predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")

#Is there a relationship between the predictor and the response. - yes
# How strong is the relationship between the predictor and the response?  - 60% relation
# Is the relationship between the predictor and the response positive or negative. - negative
# What is the predicted mpg associated with a horsepower of 98? What are the associated 95% confidence and prediction intervals? - 
# - y = 39.94-0.158x => y = 24.456 We are 95% confident that the mpg of a car with horsepower of 98 is between 14.81 to 34.12. 
# We are 95% confident that the average mpg of a car with horsepower of 98 is between 23.97 to 24.96.  

### Part 2 ###
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(auto.lin.fit, lwd = 3, col = "red")

### Part 3 ###
par(mfrow = c(2, 2))
plot(auto.lin.fit)

################################## Applied Exercise 2 ####################################################################################

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

########################################################## Applied Exercise 3 #####################################

### Part 1 ###
library(ISLR)
head(Carseats)

# Fit a multiple regression model to predict Sales using Price, Urban, and US.

carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)

contrasts(Carseats$Urban)

contrasts(Carseats$US)

### Part 2 ###
# Provide an interpretation of each coefficient in the model.
# The coefficient of -0.054459 for Price means that, for a given location (i.e. fixed values of Urban and US), 
#increasing the price of a car seat by $1 results in a decrease of sales by approximately 54.46 units, on average, 
#in the model. The coefficient of -0.021916 for UrbanYes means that, for a given carseat price point and value of US, 
#the model predicts urban areas to have approximately 22 fewer carseat sales on average compared to non-urban areas. 
#The coefficient of 1.200573 for USYes means that, for a given carseat price point and value of Urban, the model predicts that 
#stores in the United States have 1201 more carseat sales on average than stores outside the United States

### Part 3 ###
# Write out the model in equation form
# The model has the following equation.
#Y^=13.043−0.054X1−0.022X2+1.200X3
#Here, y^ is the estimated carseat sales, in thousands of car seats; x1j is the price of the carseat at the jth store, 
#in dollars; and x2j and x3j are dummy variables to represent whether or not the jth store at is located in an urban area 
#and in the United States, respectively. More concretely, x2j and x3j
# x2j = 1:0 --> location urban or not
# x3j = 1:0 --> store in US or not

### Part 4 ###
# For which of the predictors can you reject the null hypothesis H0:βj=0?
# The p-values for the intercept, Price, and USYes are all essentially zero, which provides strong evidence to reject 
#the null hypothesis H0:βj=0 for those predictors. The p-value for UrbanYes, however, is 0.936, so there is no evidence 
#to reject the null hypothesis that it has a non-zero coefficient in the true relationship between the predictors and Sales.

### Part 5 ###
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)

###  Part 6 ###
par(mfrow = c(2, 2))
plot(carseats.fit.1)

par(mfrow = c(2, 2))
plot(carseats.fit.2)

### Part 7 ###
confint(carseats.fit.2)

#### Part 8 ###
#Is there evidence of outliers or high leverage observations in the model from Part 5?
#When we look at the residuals vs. leverage plot for the model from Part 5 that I generated in Part 6, 
#we see that there are a number of observations with standardized residuals close to 3 in absolute value. 
#Those observations are possible outliers. We can also see in the same plot that there are number of high leverage 
#points with leverage values greatly exceeding the average leverage of 3/400=0.0075, though those high leverage observations 
#are not likely outliers, as they have studentized residual values with absolute value less than 2.


############################################ Applied Exercise 4 ###################################################################

### Part 1 ###
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

noint.y.onto.x = lm(y ~ x + 0)
summary(noint.y.onto.x)

# Here, beta1 = 1.99, R^2: 0.7776, F-statistics: 350.7, P = 2.2e-16, accepting Alternate hypothesis

plot(x, y)
abline(noint.y.onto.x, lwd = 3, col = "red")

### Part 2 ###
noint.x.onto.y = lm(x ~ y + 0)
summary(noint.x.onto.y)

plot(y, x)
abline(noint.x.onto.y, lwd = 3, col = "red")

### Part 3 ###
#In Part 1, simple linear regression without an intercept gave a regression line of y^=1.9939x. 
#In Part 2, simple linear regression without an intercept gave a regression line of x^=0.3911y. 
#Both regression models have the same t-statistic for β, 18.73, as well as the same R2 and F-statistic 
#values (0.7798 and 305.7, respectively). Also, as discussed above, there is the true relationship Y=2X+ϵ 
#for the regression in Part 1, which can be rearranged to get the true relationship X=12(Y−ϵ) in Part 2.

### Part 4 ###
sum.xy = sum(x * y)
sum.x2 = sum(x**2)
sum.y2 = sum(y**2)
(sqrt(100 - 1)*sum.xy)/sqrt(sum.x2*sum.y2 - sum.xy**2)
summary(noint.y.onto.x)$coefficients[, "t value"]

### Part 5 ###
# the formula for the t-statistic for the regression of Y
#onto X without an intercept obtained in Part 4 is symmetric in X and Y. Thus, it will give the same 
#value when performing regression of Y onto X as when performing regression of X onto Y. This explains 
#why we had the same t-statistic value for Part 1 and Part 2.

### Part 6 ###
withint.y.onto.x = lm(y ~ x)
summary(withint.y.onto.x)

withint.x.onto.y = lm(x ~ y)
summary(withint.x.onto.y)

summary(withint.y.onto.x)$coefficients[2, "t value"]

summary(withint.x.onto.y)$coefficients[2, "t value"]


########################## Applied Exercise 5 ######################################################################

### Part 1 ###
# Since the numerator in the formula for β^ is symmetric in X and Y, the coefficient estimate for the regression 
#of X onto Y will be same as the coefficient estimate for the regression of Y onto X when ∑j=1nx2j=∑j=1ny2j.

### Part 2 ###
set.seed(1)
x = rnorm(100)
y = 3*x + rnorm(100)

y.onto.x = lm(y ~ x + 0)
summary(y.onto.x)

x.onto.y = lm(x ~ y + 0)
summary(x.onto.y)

### Part 3 ###
set.seed(1)
x = rnorm(100)
y = x

y.onto.x = lm(y ~ x + 0)
summary(y.onto.x)

x.onto.y = lm(x ~ y + 0)
summary(x.onto.y)

set.seed(1)
x = rnorm(100)
y = sample(x)

y.onto.x = lm(y ~ x + 0)
summary(y.onto.x)

x.onto.y = lm(x ~ y + 0)
summary(x.onto.y)

############################################## Applied Exercise 6 ####################################################

### Part 1 ###
set.seed(1)
x = rnorm(100, mean = 0, sd = 1)

### Part 2 ###
eps = rnorm(100, mean = 0, sd = 0.5)

### Part 3 ###
y = -1 + 0.5*x + eps
length(y)

### Part 4 ###
plot(x, y, main = "Plot for original data (variance = .25)")

### Part 5 ###
lm.fit.orig = lm(y ~ x)
summary(lm.fit.orig)

### Part 6 ###
plot(x, y, main = "Plot for original data (variance = .25)")
abline(lm.fit.orig, lwd = 2, lty = 2, col = "red")
abline(a = -1, b = 0.5, lwd = 2, lty = 4, col = "blue")
legend(-2.25, 0.4, legend = c("Least squares regression", "Population regression"), col = c("red", "blue"), 
       lty = c(2, 4), lwd = 2, cex = 0.8)

### Part 7 ###
quad.fit = lm(y ~ x + I(x^2))
summary(quad.fit)

### Part 8 ###
eps.less.noise = rnorm(100, mean = 0, sd = 0.25)
y.less.noise = -1 + 0.5*x + eps.less.noise

plot(x, y.less.noise, main = "Plot for data with less noise (variance = .0625)")

lm.fit.less.noise = lm(y.less.noise ~ x)
summary(lm.fit.less.noise)

plot(x, y.less.noise, main = "Plot for data with less noise (variance = .0625)")
abline(lm.fit.less.noise, lwd = 2, lty = 2, col = "red")
abline(a = -1, b = 0.5, lwd = 2, lty = 4, col = "blue")
legend(-2.25, 0.4, legend = c("Least squares regression", "Population regression"), col = c("red", "blue"), 
       lty = c(2, 4), lwd = 2, cex = 0.8)

quad.fit.less.noise = lm(y.less.noise ~ x + I(x^2))
summary(quad.fit.less.noise)

### Part 9 ###
eps.more.noise = rnorm(100, mean = 0, sd = 1)
y.more.noise = -1 + 0.5*x + eps.more.noise

plot(x, y.more.noise, main = "Plot for data with more noise (variance = 1)")

lm.fit.more.noise = lm(y.more.noise ~ x)
summary(lm.fit.more.noise)

plot(x, y.more.noise, main = "Plot for data with more noise (variance = 1)")
abline(lm.fit.more.noise, lwd = 2, lty = 2, col = "red")
abline(a = -1, b = 0.5, lwd = 2, lty = 4, col = "blue")
legend(-2.25, 0.4, legend = c("Least squares regression", "Population regression"), col = c("red", "blue"), 
       lty = c(2, 4), lwd = 2, cex = 0.8)

quad.fit.more.noise = lm(y.more.noise ~ x + I(x^2))
summary(quad.fit.more.noise)

### Part 10 ###
confint(lm.fit.less.noise)

confint(lm.fit.orig)

confint(lm.fit.more.noise)


############################################# Applied Exercise 7 #####################################################

### Part 1 ###
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

### Part 2 ###
cor(x1, x2)

plot(x1, x2)

### Part 3 ###
fit_exc_7_3 = lm(y~x1 + x2)
summary(fit_exc_7_3)

### Part 4 ###
fit_exc_7_4 = lm(y~x1)
summary(fit_exc_7_4)

### Part 5 ###
fit_exc_7_5 = lm(y~x2)
summary(fit_exc_7_5)

### Part 6 ###
# Even though in Part 3 we didn't have enough evidence to reject the null hypothesis H0:β2=0
#, but had strong evidence to do so in Part 5, the results from Part 3-5 do not contradict each other. 
#This is due to the strong collinearity between x1 and x2, which increases the standard error for coeffiecient estimates
#in least squares regression to grow. This in turn reduces the power of the hypothesis test, which in this context is the 
#probability of correctly detecting that the true values of β1 and β2 are non-zero. We recall that failing to reject the 
#null hypothesis H0:βj=0 doesn't mean that the βj=0 in the actual model. It simply means that there isn't enough evidence 
#to confidently conclude βj≠0 with the data we have available to us.

### Part 7 ###
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

cor(x1, x2)
fit_exc_7_7_both = lm(y~x1 + x2)
summary(fit_exc_7_7_both)

rstudent(fit_exc_7_7_both)[101]

hatvalues(fit_exc_7_7_both)[101]

fit_exc_7_7_x1 = lm(y~x1)
summary(fit_exc_7_7_x1)

rstudent(fit_exc_7_7_x1)[101]

hatvalues(fit_exc_7_7_x1)[101]

fit_exc_7_7_x2 = lm(y~x2)
summary(fit_exc_7_7_x2)

rstudent(fit_exc_7_7_x2)[101]

hatvalues(fit_exc_7_7_x2)[101]


######################################## Applied Exercise 8 ###############################################################

### Part 1 ###
boston = read.csv("boston_corrected.csv", header = TRUE)
boston = boston[, 7:20]
head(boston)

### Part 1 ###
cmedv.fit = lm(CRIM ~ CMEDV, data = boston)
summary(cmedv.fit)

zn.fit = lm(CRIM ~ ZN, data = boston)
summary(zn.fit)

indus.fit = lm(CRIM ~ INDUS, data = boston)
summary(indus.fit)

chas.fit = lm(CRIM ~ CHAS, data = boston)
summary(chas.fit)

nox.fit = lm(CRIM ~ NOX, data = boston)
summary(nox.fit)

rm.fit = lm(CRIM ~ RM, data = boston)
summary(rm.fit)

age.fit = lm(CRIM ~ AGE, data = boston)
summary(age.fit)

dis.fit = lm(CRIM ~ DIS, data = boston)
summary(dis.fit)

rad.fit = lm(CRIM ~ RAD, data = boston)
summary(rad.fit)

tax.fit = lm(CRIM ~ TAX, data = boston)
summary(tax.fit)

ptratio.fit = lm(CRIM ~ PTRATIO, data = boston)
summary(ptratio.fit)

View(boston)
lstat.fit = lm(CRIM ~ LSTAT, data = boston)
summary(lstat.fit)

### Part 2 ###
all.fit = lm(CRIM ~ ., data = boston)
summary(all.fit)

### Part 3 ###
mult.reg.coeffs = all.fit$coefficients[-1]
uni.reg.coeffs = c(cmedv.fit$coefficients[-1], zn.fit$coefficients[-1], indus.fit$coefficients[-1],
                   chas.fit$coefficients[-1], nox.fit$coefficients[-1], rm.fit$coefficients[-1],
                   age.fit$coefficients[-1], dis.fit$coefficients[-1], rad.fit$coefficients[-1],
                   tax.fit$coefficients[-1], ptratio.fit$coefficients[-1])

plot(uni.reg.coeffs, mult.reg.coeffs)
abline(a = 0, b = 1, lty = 2, col = "blue")

mult.reg.se = summary(all.fit)$coefficients[-1, "Std. Error"]
uni.reg.se = c(summary(cmedv.fit)$coefficients[-1, "Std. Error"], summary(zn.fit)$coefficients[-1, "Std. Error"],
               summary(indus.fit)$coefficients[-1, "Std. Error"], summary(chas.fit)$coefficients[-1, "Std. Error"],
               summary(nox.fit)$coefficients[-1, "Std. Error"], summary(rm.fit)$coefficients[-1, "Std. Error"],
               summary(age.fit)$coefficients[-1, "Std. Error"], summary(dis.fit)$coefficients[-1, "Std. Error"],
               summary(rad.fit)$coefficients[-1, "Std. Error"], summary(tax.fit)$coefficients[-1, "Std. Error"],
               summary(ptratio.fit)$coefficients[-1, "Std. Error"], summary(black.fit)$coefficients[-1, "Std. Error"],
               summary(lstat.fit)$coefficients[-1, "Std. Error"])

plot(uni.reg.se, mult.reg.se)
abline(a = 0, b = 1, lty = 2, col = "blue")

data.frame(uni.reg.coeffs, mult.reg.coeffs, uni.reg.se, mult.reg.se)

### Part 4 ###
cmedv.fit.cubic = lm(CRIM ~ poly(CMEDV, 3), data = boston)
summary(cmedv.fit.cubic)

zn.fit.cubic = lm(CRIM ~ poly(ZN, 3), data = boston)
summary(zn.fit.cubic)

indus.fit.cubic = lm(CRIM ~ poly(INDUS, 3), data = boston)
summary(indus.fit.cubic)

nox.fit.cubic = lm(CRIM ~ poly(NOX, 3), data = boston)
summary(nox.fit.cubic)

rm.fit.cubic = lm(CRIM ~ poly(RM, 3), data = boston)
summary(rm.fit.cubic)

age.fit.cubic = lm(CRIM ~ poly(AGE, 3), data = boston)
summary(age.fit.cubic)

dis.fit.cubic = lm(CRIM ~ poly(DIS, 3), data = boston)
summary(dis.fit.cubic)

rad.fit.cubic = lm(CRIM ~ poly(RAD, 3), data = boston)
summary(rad.fit.cubic)

tax.fit.cubic = lm(CRIM ~ poly(TAX, 3), data = boston)
summary(tax.fit.cubic)

ptratio.fit.cubic = lm(CRIM ~ poly(PTRATIO, 3), data = boston)
summary(ptratio.fit.cubic)

black.fit.cubic = lm(CRIM ~ poly(B, 3), data = boston)
summary(black.fit.cubic)

lstat.fit.cubic = lm(CRIM ~ poly(LSTAT, 3), data = boston)
summary(lstat.fit.cubic)



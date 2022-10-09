####### Applied Exercise - 1 ##################################

library(ISLR)

head(Weekly)

### Part 1 ###

summary(Weekly)

pairs(Weekly)

plot(Weekly$Volume, ylab = "Shares traded (in billions)")

cor(Weekly[-9])

# summary: Only Volume has strong positive correlation with Year

### Part 2 ###
glm.full = glm(Direction ~ . - Year - Today, data = Weekly, family = "binomial")
summary(glm.full)

# Summary: Only Lag2 has P value which reject Null Hypothesis.

### Part 3 ###
glm.full.probs = predict(glm.full, type = "response")
glm.full.pred = rep("Down", 1089)
glm.full.pred[glm.full.probs > 0.5] = "Up"
table(glm.full.pred, Weekly$Direction)

mean(glm.full.pred == Weekly$Direction)

### Part 4 ###
train = (Weekly$Year < 2009)

glm.fit = glm(Direction ~ Lag2, data = Weekly, subset = train, family = "binomial")
summary(glm.fit)

glm.probs = predict(glm.fit, Weekly[!train, ], type = "response")
glm.pred = rep("Down", dim(Weekly[!train, ])[1])
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly[!train, ]$Direction)

mean(glm.pred == Weekly[!train, ]$Direction)

mean(Weekly[!train, ]$Direction == "Up")

### Part 5 ###
library(MASS)

lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)

lda.fit

lda.pred = predict(lda.fit, Weekly[!train, ])
table(lda.pred$class, Weekly[!train, ]$Direction)

mean(lda.pred$class == Weekly[!train, ]$Direction)

### Part 6 ###
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit

qda.pred = predict(qda.fit, Weekly[!train, ])
table(qda.pred$class, Weekly[!train, ]$Direction)

mean(qda.pred$class == Weekly[!train, ]$Direction)

### Part 7 ###
library(class)

train.X = data.frame(Weekly[train, ]$Lag2)
test.X = data.frame(Weekly[!train, ]$Lag2)
train.Direction = Weekly[train, ]$Direction

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Weekly[!train, ]$Direction)

mean(knn.pred == Weekly[!train, ]$Direction)

### Part 8 ###
## If we are only considering overall prediction accuracy, it appears that logistic regression and linear discriminant analysis 
#were equally good as the models that performed the best on this data. Quadratic discriminant analysis came in third place, and k
#-nearest neighbors with k=1 a distant fourth. As noted above in Part 7, k-nearest neighbors didn't perform any better than 
#randomly guessing, and in fact performed worse than naively predicting every week would be an up week. One thing that I would be 
#cautious of, though, is the fact that aside from KNN, the other three models very strongly preferred to predict up weeks, and 
#therefore had high false positive rates. This might be okay for an investor who is comfortable with taking more risks and has 
#can afford losing money to down weeks that were incorrectly predicted to be up weeks, but a risk-averse investor would probably 
#want to use a less aggressive model, or at least require a higher probability threshold than 50% before accepting a prediction of 
#an up week.

### Part 9 ###
weighted.lag.avg = 0.4*Weekly$Lag1 + 0.35*Weekly$Lag2 + 0.15*Weekly$Lag3 + 0.05*Weekly$Lag4 + 0.05*Weekly$Lag5
Weekly = data.frame(Weekly, weighted.lag.avg)
head(Weekly)

cor(Weekly$Today, Weekly$weighted.lag.avg)

glm.fit = glm(Direction ~ weighted.lag.avg, data = Weekly, subset = train, family = "binomial")
summary(glm.fit)

glm.probs = predict(glm.fit, Weekly[!train, ], type = "response")
glm.pred = rep("Down", dim(Weekly[!train, ])[1])
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly[!train, ]$Direction)

mean(glm.pred == Weekly[!train, ]$Direction)

lda.fit = lda(Direction ~ weighted.lag.avg, data = Weekly, subset = train)

lda.pred = predict(lda.fit, Weekly[!train, ])
table(lda.pred$class, Weekly[!train, ]$Direction)

mean(lda.pred$class == Weekly[!train, ]$Direction)

qda.fit = qda(Direction ~ weighted.lag.avg, data = Weekly, subset = train)

qda.pred = predict(qda.fit, Weekly[!train, ])
table(qda.pred$class, Weekly[!train, ]$Direction)

mean(qda.pred$class == Weekly[!train, ]$Direction)

qda.pred60 = rep("Down", dim(Weekly[!train, ])[1])
qda.pred60[qda.pred$posterior[, "Up"] > 0.6] = "Up"
table(qda.pred60, Weekly[!train, ]$Direction)

mean(qda.pred60 == Weekly[!train, ]$Direction)

train.X = data.frame(Weekly[train, "weighted.lag.avg"])
test.X = data.frame(Weekly[!train, "weighted.lag.avg"])
train.Direction = Weekly[train, "Direction"]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Weekly[!train, ]$Direction)

mean(knn.pred == Weekly[!train, ]$Direction)

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Weekly[!train, ]$Direction)

mean(knn.pred == Weekly[!train, ]$Direction)

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 5)
table(knn.pred, Weekly[!train, ]$Direction)

mean(knn.pred == Weekly[!train, ]$Direction)

######################################### Applied Exercise 2 ############################################################

### Part 1 ###
library(MASS)

library(class)

Auto = na.omit(Auto)
head(Auto)

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

mpg01 = rep(0, dim(Auto)[1])
mpg01[Auto$mpg > median(Auto$mpg)] = 1
Auto = data.frame(Auto, mpg01)
head(Auto)

### Part 2 ###
par(mfrow = c(2, 3))
plot(factor(Auto$mpg01), Auto$cylinders, ylab = "Number of engine cylinders")
plot(factor(Auto$mpg01), Auto$displacement, ylab = "Engine displacement (cubic inches)")
plot(factor(Auto$mpg01), Auto$horsepower, ylab = "Horsepower")
plot(factor(Auto$mpg01), Auto$weight, ylab = "Weight (pounds)")
plot(factor(Auto$mpg01), Auto$acceleration, ylab = "Time to reach 60mpg (seconds)")
plot(factor(Auto$mpg01), Auto$year, ylab = "Manufacture year")
mtext("Boxplots for cars with above(1) and below(0) median mpg", outer = TRUE, line = -3)

par(mfrow = c(3, 2))
plot(Auto$cylinders, Auto$mpg01, xlab = "Number of engine cylinders")
plot(Auto$displacement, Auto$mpg01, xlab = "Engine displacement (cubic inches)")
plot(Auto$horsepower, Auto$mpg01, xlab = "Horsepower")
plot(Auto$weight, Auto$mpg01, xlab = "Weight (pounds)")
plot(Auto$acceleration, Auto$mpg01, xlab = "Time to reach 60mpg (seconds)")
plot(Auto$year, Auto$mpg01, xlab = "Manufacture year")
mtext("Scatterplots for cars with above(1) and below(0) median mpg", outer = TRUE, line = -3)

plot(Auto$year, Auto$mpg)
abline(h = median(Auto$mpg), lwd = 2, col = "red")

plot(Auto$origin, Auto$mpg, xlab = "Origin", ylab = "MPG")
abline(h = median(Auto$mpg), lwd = 2, col = "red")

### Part 3 ###
set.seed(1)
train = sample(dim(Auto)[1], size = 0.75*dim(Auto)[1])

### Part 4 ###
lda.fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight + year + origin, data = Auto, subset = train)

lda.pred = predict(lda.fit, Auto[-train, ])
table(lda.pred$class, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(lda.pred$class == Auto[-train, "mpg01"])

lda.fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train)

lda.pred = predict(lda.fit, Auto[-train, ])
table(lda.pred$class, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(lda.pred$class == Auto[-train, "mpg01"])

### Part 5 ###
qda.fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight + year + origin, data = Auto, subset = train)

qda.pred = predict(qda.fit, Auto[-train, ])
table(qda.pred$class, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(qda.pred$class == Auto[-train, "mpg01"])

qda.fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train)

qda.pred = predict(qda.fit, Auto[-train, ])
table(qda.pred$class, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(qda.pred$class == Auto[-train, "mpg01"])

### Part 6 ###
glm.fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight + year + origin, data = Auto, subset = train,
              family = "binomial")
summary(glm.fit)

glm.probs = predict(glm.fit, Auto[-train, ], type = "response")
glm.pred = rep(0, dim(Auto[-train, ])[1])
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(glm.pred == Auto[-train, "mpg01"])

glm.fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train,
              family = "binomial")
summary(glm.fit)

glm.probs = predict(glm.fit, Auto[-train, ], type = "response")
glm.pred = rep(0, dim(Auto[-train, ])[1])
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))

1 - mean(glm.pred == Auto[-train, "mpg01"])

### Part 7 ###
scaled.auto = scale(Auto[, -c(8, 9, 10)])
head(scaled.auto)

cols = c("cylinders", "displacement", "horsepower", "weight", "year")
train.X = scaled.auto[train, cols]
test.X = scaled.auto[-train, cols]
train.mpg01 = Auto[train, "mpg01"]

set.seed(1)
k.vals = (1:10)*2 - 1
knn.error = rep(0, 10)
knn.tables = list()
for (i in 1:10){
  knn.pred = knn(train.X, test.X, train.mpg01, k = 2*i - 1)
  knn.tables[[k.vals[i]]] = table(knn.pred, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))
  knn.error[i] = 1 - mean(knn.pred == Auto[-train, "mpg01"])
}
cbind(k.vals, knn.error)

knn.tables[[1]]

cols = c("cylinders", "displacement", "horsepower", "weight")
train.X = scaled.auto[train, cols]
test.X = scaled.auto[-train, cols]
train.mpg01 = Auto[train, "mpg01"]

set.seed(1)
k.vals = (1:10)*2 - 1
knn.error = rep(0, 10)
knn.tables = list()
for (i in 1:10){
  knn.pred = knn(train.X, test.X, train.mpg01, k = 2*i - 1)
  knn.tables[[k.vals[i]]] = table(knn.pred, Auto[-train, "mpg01"], dnn = c("Predicted", "Actual"))
  knn.error[i] = 1 - mean(knn.pred == Auto[-train, "mpg01"])
}
cbind(k.vals, knn.error)

knn.tables[[1]]

############################### Applied Exercise 3 #############################################################

### Part 1 ###
Power = function(){
  print(2^3)
}

Power()

### Part 2 ###
Power2 = function(x, a){
  print(x^a)
}

Power2(3, 8)

### Part 3 ###
Power2(10, 3)

Power2(8, 17)

Power2(131, 3)

### Part 4 ###
Power3 = function(x, a){
  return(x^a)
}

result = Power3(5, 3)
result

### Part 5 ###
plot(1:10, Power3(1:10, 2), xlab = "x", ylab = "f(x)", main = "Plot of x vs x^2")

plot(1:10, Power3(1:10, 2), xlab = "x", ylab = "f(x)", main = "Log-scale plot of x vs x^2", log = "y")

### Part 6 ###
PlotPower = function(x, a, log = ""){
  plot(x, x^a, xlab = "x", ylab = paste("x^", a, sep = ""), log = log)
}

PlotPower(1:10, 3)

PlotPower(1:10, 3, log = "xy")

############################### Applied Exercise 4 ######################################################################
### Part 1 ###
boston = read.csv("boston_corrected.csv", header = TRUE)
boston = boston[, 7:20]
head(boston)

crim.med = rep("No", dim(boston)[1])
crim.med[boston$CRIM > median(boston$CRIM)] = "Yes"
boston = data.frame(boston, crim.med)
head(boston)

cor(boston[, -15])[, "CRIM"]

par(mfrow = c(2, 3))
plot(boston$crim.med, boston$CMEDV, ylab = "Median home value ($1000)")
plot(boston$crim.med, boston$ZN, ylab = "Proportion of land zoned for 25000+ sq ft lots")
plot(boston$crim.med, boston$INDUS, ylab = "Proportion of non-retail business acres")
plot(boston$crim.med, boston$NOX, ylab = "Nitric oxides concentration (parts per 10 million)")
plot(boston$crim.med, boston$RM, ylab = "Average rooms per home")
plot(boston$crim.med, boston$AGE, ylab = "Proportion of homes built before 1940")
mtext("Boxplots for towns with above (Yes) and below (No) median crime rate", outer = TRUE, line = -3)

par(mfrow = c(2, 3))
plot(boston$crim.med, boston$DIS, ylab = "Weighted distance to Boston employment centers")
plot(boston$crim.med, boston$RAD, ylab = "Index of accessibility to radial highways")
plot(boston$crim.med, boston$TAX, ylab = "Property tax rate (USD per $10000)")
plot(boston$crim.med, boston$PTRATIO, ylab = "Pupil-teacher ratio")
plot(boston$crim.med, boston$B, ylab = "1000*(Proportion of black residents - 0.63)^2")
plot(boston$crim.med, boston$LSTAT, ylab = "Proportion lower socioeconomic status population")
mtext("Boxplots for towns with above (Yes) and below (No) median crime rate", outer = TRUE, line = -3)

set.seed(312)
train = sample(dim(boston)[1], size = 0.75*dim(Boston)[1])

glm.fit = glm(crim.med ~ . - CRIM - CHAS, data = boston, subset = train, family = "binomial")


glm.probs = predict(glm.fit, boston[-train, ], type = "response")
glm.pred = rep("No", dim(boston[-train, ])[1])
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(glm.pred == boston[-train, "crim.med"])

glm.fit = glm(crim.med ~ CMEDV + INDUS + NOX + AGE + DIS + RAD + TAX + B + LSTAT,
              data = boston, subset = train, family = "binomial")
glm.probs = predict(glm.fit, boston[-train, ], type = "response")
glm.pred = rep("No", dim(boston[-train, ])[1])
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(glm.pred == boston[-train, "crim.med"])

lda.fit = lda(crim.med ~ . - CRIM - CHAS, data = boston, subset = train)
lda.pred = predict(lda.fit, boston[-train, ])
table(lda.pred$class, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(lda.pred$class == boston[-train, "crim.med"])

lda.fit = lda(crim.med ~ CMEDV + INDUS + NOX + AGE + DIS + RAD + TAX + B + LSTAT, data = boston, subset = train)
lda.pred = predict(lda.fit, boston[-train, ])
table(lda.pred$class, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(lda.pred$class == boston[-train, "crim.med"])

qda.fit = qda(crim.med ~ . - CRIM - CHAS, data = boston, subset = train)
qda.pred = predict(qda.fit, boston[-train, ])
table(qda.pred$class, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(qda.pred$class == boston[-train, "crim.med"])

qda.fit = qda(crim.med ~ CMEDV + INDUS + NOX + AGE + DIS + RAD + TAX + B, data = boston, subset = train)
qda.pred = predict(qda.fit, boston[-train, ])
table(qda.pred$class, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))

1 - mean(qda.pred$class == boston[-train, "crim.med"])

scaled.boston = scale(boston[, -c(2, 5, 15)])
head(scaled.boston)

train.X = scaled.boston[train, ]
test.X = scaled.boston[-train, ]
train.crim.med = boston[train, "crim.med"]

set.seed(312)
k.vals = (1:10)*2 - 1
knn.error = rep(0, 10)
knn.tables = list()
for (i in 1:10){
  knn.pred = knn(train.X, test.X, train.crim.med, k = 2*i - 1)
  knn.tables[[k.vals[i]]] = table(knn.pred, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))
  knn.error[i] = 1 - mean(knn.pred == boston[-train, "crim.med"])
}
cbind(k.vals, knn.error)

knn.tables[[3]]

cols = c("CMEDV", "INDUS", "NOX", "AGE", "DIS", "RAD", "TAX", "B")
train.X = scaled.boston[train, cols]
test.X = scaled.boston[-train, cols]
train.crim.med = boston[train, "crim.med"]

set.seed(312)
k.vals = (1:10)*2 - 1
knn.error = rep(0, 10)
knn.tables = list()
for (i in 1:10){
  knn.pred = knn(train.X, test.X, train.crim.med, k = 2*i - 1)
  knn.tables[[k.vals[i]]] = table(knn.pred, boston[-train, "crim.med"], dnn = c("Predicted", "Actual"))
  knn.error[i] = 1 - mean(knn.pred == boston[-train, "crim.med"])
}
cbind(k.vals, knn.error)

knn.tables[[3]]


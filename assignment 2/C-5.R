################################ Applied Exercise 1 ###############################################
library(ISLR)
head(Default)
set.seed(312)

### Part 1 ###
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

### Part 2 ###
train = sample(dim(Default)[1], 0.75*dim(Default)[1])

glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])
# 2.84 % of set are misclassified

### Part 3 ###
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

1 - mean(Default[-train, "default"] == "No")

### Part 4 ###
with.student = rep(0, 50)
without.student = rep(0, 50)
for (i in 1:50){
  train = sample(dim(Default)[1], 0.75*dim(Default)[1])
  with.student.fit = glm(default ~ ., data = Default, subset = train, family = "binomial")
  without.student.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
  with.student.probs = predict(with.student.fit, Default[-train, ], type = "response")
  without.student.probs = predict(without.student.fit, Default[-train, ], type = "response")
  with.student.preds = rep("No", dim(Default)[1])
  without.student.preds = rep("No", dim(Default)[1])
  with.student.preds[with.student.probs > 0.5] = "Yes"
  without.student.preds[without.student.probs > 0.5] = "Yes"
  with.student[i] = mean(with.student.preds != Default[-train, "default"])
  without.student[i] = mean(without.student.preds != Default[-train, "default"])
}
difference = with.student - without.student
errors = data.frame(with.student, without.student, difference)
mean(errors$difference)

################################################# Applied Exercise 2 ###########################################################

library(boot)
set.seed(312)

### Part 1 ###
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

### Part 2 ###
boot.fn = function(data, index){
  coefs = coef(glm(default ~ income + balance, data = data, subset = index, 
                   family = "binomial"))[c("income", "balance")]
  return(coefs)
}

### Part 3 ###
boot(Default, boot.fn, 1000)

### Part 4 ###
# The standard errors obtained by the bootstrap appear to be 
#a quite close to those obtained using the statistical formulas underlying the glm() function. 
#This suggests that the data satisfies the underlying assumptions of a logistic regression model: the responses Yi
#are independent random variables coming from Bernoulli distributions with probabilities Pi, and the log-odds corresponding 
#to Pi is a linear combination of the predictors.


############################################ Applied Exercise 3 #######################################################################
head(Weekly)

### Part 1 ###
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(glm.fit)

### Part 2 ###
glm.fit.loo = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial", subset = c(-1))
summary(glm.fit.loo)

### Part 3 ###
predict(glm.fit.loo, Weekly[1, ])
Weekly[1, ]

### Part 4 ###
n = dim(Weekly)[1]
errors = rep(0, n)
for (i in 1:n){
  glm.fit.loo = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial", subset = c(-i))
  pred = "Down"
  if (predict(glm.fit.loo, Weekly[i, ], type = "response") > 0.5){
    pred = "Up"
  }
  if (pred != Weekly[i, "Direction"]){
    errors[i] = 1
  }
}

### Part 5 ###
mean(errors)

glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
cost = function(r, pi) mean(abs(r-pi)> 0.5)
cv.err = cv.glm(Weekly, glm.fit, cost = cost)
cv.err$delta[1]



########################################### Applied Exercise 4 #############################################################

### Part 1 ###
set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

### Part 2 ###
plot(x, y, xlim = range(-3:3), ylim = range(-15:4))
par(new = TRUE)
curve(x - 2*x^2, from = -3, to = 3, xlim = range(-3:3), ylim = range(-15:4), xlab = "", ylab = "", col = "red")

### Part 3 ###
set.seed(312)

xy.data = data.frame("pred" = x, "resp" = y)

deg1.fit = glm(resp ~ pred, data = xy.data)
deg1.err = cv.glm(xy.data, deg1.fit)
deg1.err$delta[1]

deg2.fit = glm(resp ~ poly(pred, 2), data = xy.data)
deg2.err = cv.glm(xy.data, deg2.fit)
deg2.err$delta[1]

deg3.fit = glm(resp ~ poly(pred, 3), data = xy.data)
deg3.err = cv.glm(xy.data, deg3.fit)
deg3.err$delta[1]

deg4.fit = glm(resp ~ poly(pred, 4), data = xy.data)
deg4.err = cv.glm(xy.data, deg4.fit)
deg4.err$delta[1]

### Part 4 ###
set.seed(42)

deg1.fit = glm(resp ~ pred, data = xy.data)
deg1.err = cv.glm(xy.data, deg1.fit)
deg1.err$delta[1]

deg2.fit = glm(resp ~ poly(pred, 2), data = xy.data)
deg2.err = cv.glm(xy.data, deg2.fit)
deg2.err$delta[1]

deg3.fit = glm(resp ~ poly(pred, 3), data = xy.data)
deg3.err = cv.glm(xy.data, deg3.fit)
deg3.err$delta[1]

deg4.fit = glm(resp ~ poly(pred, 4), data = xy.data)
deg4.err = cv.glm(xy.data, deg4.fit)
deg4.err$delta[1]

### Part 5 ###
# The quadratic model in Part 3 had the smallest LOOCV error, though the cubic and quadratic models also had close 
#LOOCV error values, especially compared to the much larger LOOCV error value for the linear model. This is what 
#I expected, since the true model used to produce the simulated data was a quadratic one.

### Part 6 ###
summary(deg1.fit)$coef

summary(deg2.fit)$coef

summary(deg3.fit)$coef

summary(deg4.fit)$coef



##################################### Applied Exercise 5 #####################################################################

boston = read.csv("boston_corrected.csv", header = TRUE)
boston = boston[, 7:20]
head(boston)

### Part 1 ###
mean(boston$CMEDV)

### Part 2 ###
sample.sd = sd(boston$CMEDV)
sample.sem = sample.sd / sqrt(dim(boston)[1])
sample.sem

### Part 3 ###
set.seed(312)

boot.fn = function(data, index){
  return(mean(data[index, "CMEDV"]))
}
boot(boston, boot.fn, 10000)

### Part 4 ###
t.test(boston$CMEDV)

### Part 5 ###
median(boston$CMEDV)

### Part 6 ###
set.seed(312)

boot.fn = function(data, index){
  return(median(data[index, "CMEDV"]))
}

boot(boston, boot.fn, 10000)

### Part 7 ###
quantile(boston$CMEDV, c(0.1))

### Part 8 ###
set.seed(312)

boot.fn = function(data, index){
  return(quantile(data[index, "CMEDV"], 0.1))
}

boot(boston, boot.fn, 10000)


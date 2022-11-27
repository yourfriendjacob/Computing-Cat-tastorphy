#install.packages("sos")
#install.packages('e1071', dependencies=TRUE)
library(e1071)
library(sos)
#findFn('tune', maxPages = 1)
#pS <- findFn(string = 'tune', maxPages = 1)

#a
set.seed(1)
x.one <- runif(500, 0, 90)
y.one <- runif(500, x.one + 10, 100)
x.one.noise <- runif(50, 20, 80)
y.one.noise <- 5/4 * (x.one.noise - 10) + 0.1
x.zero <- runif(500, 10, 100)
y.zero <- runif(500, 0, x.zero - 10)
x.zero.noise <- runif(50, 20, 80)
y.zero.noise <- 5/4 * (x.zero.noise - 10) - 0.1
class.one <- seq(1, 550)
x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)
plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)


#b

set.seed(2)
z <- rep(0, 1100)
z[class.one] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
tune.out <- tune(svm, z ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)
#A cost of 10000 is the best parameter on the training observations, it correctly classifies the training points
data.frame(cost = tune.out$performance$cost, misclass = tune.out$performance$error * 1100)

#c

x.test <- runif(1000, 0, 100)
class.one <- sample(1000, 500)
y.test <- rep(NA, 1000)

for (i in class.one) {
  y.test[i] <- runif(1, x.test[i], 100)
}

for (i in setdiff(1:1000, class.one)) {
  y.test[i] <- runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)
set.seed(3)
z.test <- rep(0, 1000)
z.test[class.one] <- 1
data.test <- data.frame(x = x.test, y = y.test, z = as.factor(z.test))
costs <- c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.err <- rep(NA, length(costs))
for (i in 1:length(costs)) {
  svm.fit <- svm(z ~ ., data = data, kernel = "linear", cost = costs[i])
  pred <- predict(svm.fit, data.test)
  test.err[i] <- sum(pred != data.test$z)
}
data.frame(cost = costs, misclass = test.err)

#Costs of 1, 5 or 10 perform better on test observations

#d

#A large cost correctly classifies point but also tries to classify the noise points correctly as well resulting of an overfitting issue in the train data.On the other hand, a small cost makes some errors on the noise points but performs better on test data


#install.packages("ggplot2")
#library(ggplot2)
#install.packages("latex2exp")
#library(latex2exp)
#install.packages("magrittr")
#library(magrittr)



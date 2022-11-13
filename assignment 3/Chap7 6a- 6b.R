rm(list = ls())
set.seed(1)
library(ISLR)
library(boot)

# create a container to test errors
cv.MSE <- NA

# loop over powers of age
for (i in 1:15) {
  glm.fit <-  glm(wage ~ poly(age, i), data = Wage)
# cross-validation on the cv.glm and keep the cv test error
  cv.MSE[i] <-  cv.glm(Wage, glm.fit, K = 10)$delta[1]
}
# inspect results object
cv.MSE
# illustrate results with a line plot connecting the cv.error dots
plot( x = 1:15, y = cv.MSE, xlab = "power of age", ylab = "CV error", 
      type = "b", pch = 19, lwd = 2, bty = "n", 
      ylim = c( min(cv.MSE) - sd(cv.MSE), max(cv.MSE) + sd(cv.MSE) ) )

# horizontal line to show complexity
abline(h = min(cv.MSE) + sd(cv.MSE) , lty = "dotted")

# mark the minimum with x
points( x = which.min(cv.MSE), y = min(cv.MSE), col = "red", pch = "X", cex = 1.5 )



#fit the models again with higher powers of age for anova.
# create container for the models
models <- vector("list", length(cv.MSE))
# fit all 15 models
for( a in 1:length(cv.MSE)){
  models[[a]] <- glm(wage ~ poly(age, a), data = Wage)
}
# f-test
anova(models[[1]], models[[2]], models[[3]], models[[4]], models[[5]], models[[6]],
      models[[7]], models[[8]], models[[9]], models[[10]], models[[11]], models[[12]],
      models[[13]], models[[14]], models[[15]], test = "F")

#  plot the results of the polynomial fit.
plot(wage ~ age, data = Wage, col = "darkgrey",  bty = "n")
agelims <-  range(Wage$age)
age.grid <-  seq(from = agelims[1], to = agelims[2])
lm.fit <-  lm(wage ~ poly(age, 2), data = Wage)
lm.pred <-  predict(lm.fit, data.frame(age = age.grid), se = TRUE)
# mean of all prediction

lines(x = age.grid , y = lm.pred$fit, col = "blue", lwd = 2)
# uncertainty bands
matlines( x = age.grid, y = cbind( lm.pred$fit + 2*lm.pred$se.fit, lm.pred$fit - 2*lm.pred$se.fit),
          lty = "dashed", col = "blue")


#Q2-

cv.error <-  NA
# for each cut perform 10-fold cross-validation
for (i in 2:15) {
  Wage$age.cut <-  cut(Wage$age, i)
  lm.fit <-  glm(wage ~ age.cut, data = Wage)
  cv.error[i] <-  cv.glm(Wage, lm.fit, K = 10)$delta[1]
}


# the first element of cv.error is NA because we started our loop at 2
plot(2:15, cv.error[-1], xlab = "Number of cuts", ylab = "CV error", 
     type = "b", pch = 19, lwd = 2, bty ="n")

# horizontal line for 1se to less complexity
abline(h = min(cv.error, na.rm = TRUE) + sd(cv.error, na.rm = TRUE) , lty = "dotted")

# highlight minimum
points( x = which.min(cv.error), y = min(cv.error, na.rm = TRUE), col = "red", pch = "X", cex = 1.5 )

#Based on the Cross validation graph, the test error is minimized at k=8 knots. The most parsimonious model has k=4 knots 
#and splits the data into 5 distinct sections
# train the entire data with step function using 4 cuts and plot it.
lm.fit <-  glm(wage ~ cut(age, 4), data = Wage)
agelims <-  range(Wage$age)
age.grid <-  seq(from = agelims[1], to = agelims[2])
lm.pred <-  predict(lm.fit, data.frame(age = age.grid), se = TRUE)
plot(wage ~ age, data = Wage, col = "darkgrey", bty = "n")
lines(age.grid, lm.pred$fit, col = "red", lwd = 2)
matlines(age.grid, cbind( lm.pred$fit + 2* lm.pred$se.fit,
                          lm.pred$fit - 2* lm.pred$se.fit),
         col = "red", lty ="dashed")
  

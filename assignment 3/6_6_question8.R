#Jacob Sauther
#CS5565
#assignment 3, question 8, 6.6
#Fall 2022

# import needed for regsubsets
library(leaps)
library(magrittr)
library(ggplot2)
library(dplyr)
library(scales)
library(glmnet)

# Create x and the noise set
set.seed(42)
X = rnorm(100)
noise = rnorm(100)

# Generate Y
Y = 5 + 24*X + 66*X^2 + 2*X^3 + noise

# Best subset selection
X_mat = poly(X, degree = 10, raw = T, simple = T)
model = regsubsets(y = Y, x = X_mat, nvmax = 10, method = "exhaustive")
model_sum = summary(model)
model_sum

# Mallows CP
data.frame(cp = model_sum$cp, subset_size = 1:10) %>%
  mutate(min_cp = as.numeric(min(cp) == cp)) %>%
  ggplot(aes(x = subset_size, y = cp)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_cp))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - Mallows CP", x = "Subset Size", y = "CP")

# 7 variables selected

# BIC
data.frame(bic = model_sum$bic, subset_size = 1:10) %>%
  mutate(min_bic = as.numeric(min(bic) == bic)) %>%
  ggplot(aes(x = subset_size, y = bic)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_bic))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - BIC", x = "Subset Size", y = "BIC")

# 5 variables selected

# Adjusted R-squared
data.frame(adj_r2 = model_sum$adjr2, subset_size = 1:10) %>%
  mutate(max_adj_r2 = as.numeric(max(adj_r2) == adj_r2)) %>%
  ggplot(aes(x = subset_size, y = adj_r2)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(max_adj_r2))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - Adjusted R^2", x = "Subset Size", y = "Adjusted R^2")

# 7 variables selected

# so when there are 7 predictors selected we get X1, X2, X5, X7, X8, X9, and X10
# When 5 are selected we get X1, X2, X5, X7, X9

# the coefficients are given for 5 to be
coef(model, id = 5)
# the coefficients are given for 7 to be
coef(model, id = 7)

# BIC seems to be closer to the original Coefficients. The others are not too far off, but BIC is closer and has less predictors.

#forward stepwise:
model = regsubsets(y = Y, x = X_mat, nvmax = 10, method = "forward")
model_sum = summary(model)
model_sum

# Mallows CP
which.min(model_sum$cp) #[1] 6

# BIC 
which.min(model_sum$bic) #[1] 6

# Adjusted R^2
which.max(model_sum$adjr2) #[1] 8

# the coefficients are given for 6 to be
coef(model, id = 6)
# the coefficients are given for 8 to be
coef(model, id = 8)

# 6 appears to have closer coefficients to the original values. 6 Would be what I pick. It may not matter that much between Mallows CP and BIC though.

#backward stepwise:
model = regsubsets(y = Y, x = X_mat, nvmax = 10, method = "backward")
model_sum = summary(model)
model_sum

# Mallows CP
which.min(model_sum$cp) #[1] 7

# BIC 
which.min(model_sum$bic) #[1] 6

# Adjusted R^2
which.max(model_sum$adjr2) #[1] 8

# the coefficients are given for 7 to be
coef(model, id = 7)
# the coefficients are given for 6 to be
coef(model, id = 6)
# the coefficients are given for 8 to be
coef(model, id = 8)

# here adjusted R^2 gets the closest to the original coefficients. The rest are close but not as close as R^2. Though it has more predictors than the other methods so far.
# Best subset selection seemed to get the closest without adding too many predictors. Though it is still a little off.

# Lasso Selection
set.seed(1, sample.kind = "Rounding")
model_lasso = cv.glmnet(y = Y, x = X_mat, alpha = 1, lambda = 10^seq(1, -2, length=100), standardize = TRUE, nfolds = 5)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10') + 
  scale_y_continuous(trans = 'log10') + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

# best value of lambda is roughly 0.02
model_lasso$lambda.min

best_lambda = glmnet(y = Y, x = X_mat, alpha = 1)
predict(best_lambda, s = model_lasso$lambda.min, type = "coefficients")
# Here it appears that to coefficients are very close to our original values. The values are only just barely off the values. We also have the same number of predictors
# as in the original function.

# New function
Y = 10 + 356*X^7 + noise

# best subset selection
X_mat = poly(X, degree = 10, raw = T, simple = T)
model = regsubsets(y = Y, x = X_mat, nvmax = 10, method = "exhaustive")
model_sum = summary(model)
model_sum

# Mallows CP
data.frame(cp = model_sum$cp, subset_size = 1:10) %>%
  mutate(min_cp = as.numeric(min(cp) == cp)) %>%
  ggplot(aes(x = subset_size, y = cp)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_cp))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - Mallows CP", x = "Subset Size", y = "CP")
# 6 predictors

# BIC
data.frame(bic = model_sum$bic, subset_size = 1:10) %>%
  mutate(min_bic = as.numeric(min(bic) == bic)) %>%
  ggplot(aes(x = subset_size, y = bic)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_bic))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - BIC", x = "Subset Size", y = "BIC")
# 4 predictors

# Adjusted R-squared
data.frame(adj_r2 = model_sum$adjr2, subset_size = 1:10) %>%
  mutate(max_adj_r2 = as.numeric(max(adj_r2) == adj_r2)) %>%
  ggplot(aes(x = subset_size, y = adj_r2)) +
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(max_adj_r2))) + 
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) + 
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Best Subset Selection - Adjusted R^2", x = "Subset Size", y = "Adjusted R^2")
# 7 predictors

# coefficients for the above
coef(model, id = 6)
coef(model, id = 4)
coef(model, id = 7)
# here the only model that gets even remotely close to the original is the adjust r^2 model. This one is very very close, but has other coefficients which 
# throw if off from being perfect.

#Lasso Selection
set.seed(1, sample.kind = "Rounding")
model_lasso = cv.glmnet(y = Y, x = X_mat, alpha = 1, lambda = 10^seq(1, -2, length=100), standardize = TRUE, nfolds = 5)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10') + 
  scale_y_continuous(trans = 'log10') + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

# best value of lambda is 10
model_lasso$lambda.min

best_lambda = glmnet(y = Y, x = X_mat, alpha = 1)
predict(best_lambda, s = model_lasso$lambda.min, type = "coefficients")

# this model is also not far off, but the first coefficient very off. In the original it was 10, but was predicted to ~-362.32.
# the other coefficient is very close. Originally it was 365 and it was predicted to be ~345.62




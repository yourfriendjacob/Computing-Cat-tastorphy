# Assignment 4
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001
# Feng Zheng

# ISLR 9.7 Problem 5
# a: code given by the question
set.seed(1)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1*(x1^2 - x2^2 > 0)
df <- data.frame(x1, x2, y = factor(y))

# b: plot the observations, colored according to their class labels.
ggplot(df, aes(x = x1, y = x2, col = y)) + geom_point() + scale_x_continuous(limits = c(-0.6, 0.6)) +
  scale_y_continuous(limits = c(-0.6, 0.6)) + scale_color_manual(values = c("red", "blue")) + 
  theme(legend.position = "none") + labs(x = TeX(r'($X_1$)'), y = TeX(r'($X_2$)'))

# c: fit a logistic regression model to the data
model_logistic_linear <- glm(y ~ x1 + x2, family = "binomial")
df$pred_logistic_linear <- ifelse(predict(model_logistic_linear, type = "response") > 0.5, 1, 0)
summary(model_logistic_linear)

# d: apply model to the training data in order to obtain a predicted class label for each training observation
table(df$pred_logistic_linear)
grid <- expand.grid(x1 = seq(-0.6, 0.6, length.out = 200), x2 = seq(-0.6, 0.6, length.out = 200))
grid$class_pred <- ifelse(predict(model_logistic_linear, newdata = grid, type = "response") > 0.5, 1, 0)

# plot it
ggplot(grid, aes(x = x1, y = x2)) + geom_tile(aes(fill = factor(class_pred)), alpha = 0.5) +
  geom_point(data = df, aes(col = y)) + scale_color_manual(values = c('red', 'blue')) + 
  labs(fill = "Predicted:", 
       shape = "Support Vector:", 
       col = "Actual:", 
       x = TeX(r'($X_1$)'), 
       y = TeX(r'($X_2$)'))

# e: fit a logistic regression model to the data using non-linear functions of X1 and X2 as predictors
model_logistic_nonlinear <- glm(y ~ x1 + x2 + I(x1^2) + I(x2^2), family = "binomial")
df$pred_logistic_nonlinear <- ifelse(predict(model_logistic_nonlinear, type = "response") > 0.5, 1, 0)
summary(model_logistic_nonlinear)

# f: logistic regression non-linear result
grid <- expand.grid(x1 = seq(-0.6, 0.6, length.out = 200), x2 = seq(-0.6, 0.6, length.out = 200))
grid$class_pred <- ifelse(predict(model_logistic_nonlinear, newdata = grid, type = "response") > 0.5, 1, 0)

# plot
ggplot(grid, aes(x = x1, y = x2)) + 
  geom_tile(aes(fill = factor(class_pred)), alpha = 0.5) +
  geom_point(data = df, aes(col = y)) +
  scale_color_manual(values = c('red', 'blue')) + 
  labs(fill = "Predicted:", 
       shape = "Support Vector:", 
       col = "Actual:", 
       x = TeX(r'($X_1$)'), 
       y = TeX(r'($X_2$)'))

# g: do the samething with svm
svm_linear <- svm(y ~ x1 + x2, data = df, kernel = "linear", scale = FALSE, cost = 0.001)
df$pred_svm_linear <- predict(svm_linear, df)
grid <- expand.grid(x1 = seq(-0.6, 0.6, length.out = 200), x2 = seq(-0.6, 0.6, length.out = 200))
grid$class_pred <- predict(svm_linear, grid)

# plot
ggplot(grid, aes(x = x1, y = x2)) + 
  geom_tile(aes(fill = class_pred), alpha = 0.5) +
  geom_point(data = df, aes(col = y)) +
  scale_color_manual(values = c("red", "blue")) + 
  labs(fill = "Predicted:", 
       shape = "Support Vector:", 
       col = "Actual:", 
       x = TeX(r'($X_1$)'), 
       y = TeX(r'($X_2$)'))

# h: fit a SVM using a non-linear kernel to the data
set.seed(2)
svm_radial_tune <- tune(svm, y ~ x1 + x2, data = df, kernel = "radial", scale = FALSE, ranges = list(cost = c(10^c(-3:8)), gamma = 10^c(-2:2)))
svm_radial <- svm_radial_tune$best.model
df$pred_svm_nonlinear <- predict(svm_radial, df)

svm_radial_tune$performances %>%
  rename(CV_error = error) %>%
  mutate(min_CV_error = case_when(cost == svm_radial_tune$best.parameters$cost & gamma == svm_radial_tune$best.parameters$gamma ~ 1, T ~ 0)) %>%
  ggplot(aes(x = cost, y = CV_error, col = factor(gamma))) + 
  geom_line() + 
  geom_point(aes(shape = factor(min_CV_error)), show.legend = F, size = 3) + 
  scale_shape_manual(values = c(20, 19)) +
  scale_x_continuous(trans = 'log10', breaks = 10^c(-3:8), minor_breaks = NULL, labels = paste0("10^", c(-3:8))) + 
  scale_y_continuous(trans = 'log10', labels = percent_format()) +
  theme(legend.position = "bottom", 
        axis.text.x = ggtext::element_markdown()) +
  labs(title = "Generated Data - SVM (Radial Kernel)", 
       subtitle = "Selecting cost & gamma parameters using cross-validation",
       x = "Cost", 
       y = "CV Error", 
       col = "Gamma:")

# i: comment result
# Logistic regression achieve the same performance as SVM only when the model is well-specified.
# On otherhand, SVM do not need to specify anything more than selection of the cost and gamma parameters
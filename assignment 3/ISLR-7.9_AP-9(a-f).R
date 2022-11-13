# 9. APPLIED: The Boston Dataset (Polynomial Regression, Splines)

# This question uses the variables dis (the weighted mean of distances to five Boston employment centers) and 
#nox (nitrogen oxides concentration in parts per 10 million) from the Boston data. We will treat dis as the predictor and nox as the response.


#(a) Cubic Polynomial

#Q: Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.

#A: I fit the model, providing the model summary():

model <- lm(nox ~ poly(dis, 3, raw = T), data = Boston)
summary(model)

#To plot the fit in ggplot, we don’t actually need to create a line of predictions and overlay it onto the data. 
#As I’ve done previously, I simply pass the formula into the geom_smooth() function:

ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3, raw = T)") + 
  labs(title = "Boston Dataset - Polynomial Regression",
       subtitle = "Predicting 'nox' with a cubic polynomial of 'dis'")  

# (b) Polynomial Regression - Training MSE

# Q: Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report the associated residual sum of squares.

#A: Degrees 1 to 10 and plotted below. Cubic & quartic fits visually seem to be the most reasonable (they look virtually identical):

library(gridExtra)
g1 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 1, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 1")

g2 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 2")

g3 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 3")

g4 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 4, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 4")

g5 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 5, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 5")

g6 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 6, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 6")

g7 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 7, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 7")

g8 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 8, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 8")

g9 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 9, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 9")

g10 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 10, raw = T)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Polynomial Degree = 10")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol = 2)

# The loop below fits these models and assesses the training MSE of the fits. I plot the results:

train_MSE <- c()

for (i in 1:10) {
  model_temp <- lm(nox ~ poly(dis, i, raw = T), data = Boston)
  train_MSE[i] <- mean(model_temp$residuals^2)
}

data.frame(degree = 1:10, train_MSE) %>%
  mutate(min_train_MSE = as.numeric(min(train_MSE) == train_MSE)) %>%
  ggplot(aes(x = degree, y = train_MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_train_MSE))) +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.0002)) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Boston Dataset - Polynomial Regression",
       subtitle = "Selecting the 'dis' polynomial degree with training MSE",
       x = "Degree",
       y = "Training MSE")  

# (c) Polynomial Regression - Tuning Degree (CV MSE)

#Q: Perform cross-validation or another approach to select the optimal degree for the polynomial, and explain your results.

# A:I perform 5 repeats of 10-fold cross-validation to provide an out-of-sample error estimate for polynomial regression from degrees 1 to 10.
#The selected model was degree 3, which makes sense from the visuals above; it appeared to be flexible enough to model the ‘true relationship’ without fitting to noise like the higher-order polynomials.


library(caret)
library(mlbench)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = custom_regression_metrics)

CV_MSE <- c()

set.seed(159)

for (i in 1:10) {
  model_temp <- train(y = Boston$nox,
                      x = poly(Boston$dis, i, raw = T, simple = T),
                      method = "lm",
                      metric = "MSE",
                      trControl = ctrl)
  CV_MSE[i] <- model_temp$results$MSE
}

data.frame(degree = 1:10, CV_MSE = CV_MSE) %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = degree, y = CV_MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) +
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 0.03, 0.002)) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Boston Dataset - Polynomial Regression",
       subtitle = "Selecting the 'dis' polynomial degree with cross-validation MSE",
       x = "Degree",
       y = "Cross-Validation MSE")

# (d) Different Splines & Knots

# Q: Use the bs() function to fit a regression spline to predict nox using dis. Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.

# A:1. The direct and literal answer to this is that it is a bit of a trick question (if we are to stay consistent with the definitions given in the text), and that we need to fit a simple cubic polynomial to the data with 0
# knots. If you have a cubic spline with K=1 knot ξ , the most direct representation is via: y=β0+β1x+β2x2+β3x3+β4(x−ξ)3+
#We have to estimate 5 coefficients, which results in 5 degrees of freedom. A cubic spline with K knots has K+4 degrees of freedom… so to get a fit with 4 degrees of freedom, we need K=0
#knots!


ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_vline(xintercept = attr(bs(Boston$dis, df = 3),"knots"), col = "grey55", linetype = "dashed") +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3)", aes(col = "Orthogonal Cubic")) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3, raw = T)", aes(col = "Raw Cubic")) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 3)", aes(col = "B-spline (no knots)")) + # note the identical fit to the cubics
  coord_cartesian(ylim = c(0.3, 0.9)) +
  theme(legend.position = "bottom") +
  labs(title = "Different Representations, Identical Regression Lines", col = "Fit:")

head(poly(Boston$dis, 3))

head(poly(Boston$dis, 3, raw = T))

head(bs(Boston$dis, df = 3))

identical(predict(lm(nox ~ poly(dis, 3), Boston)) %>% round(10), 
          predict(lm(nox ~ poly(dis, 3, raw = T), Boston)) %>% round(10), 
          predict(lm(nox ~ bs(dis, df = 3), Boston)) %>% round(10))

spline2_knots <- attr(bs(Boston$dis, df = 7), "knots")
spline2_knots

spline3_knots <- attr(bs(Boston$dis, df = 4), "knots")
spline3_knots

head(ns(Boston$dis, df = 3))

spline4_knots <- attr(ns(Boston$dis, df = 3), "knots")
spline4_knots

spline4_boundary_knots <- attr(ns(Boston$dis, df = 3), "Boundary.knots")
spline4_boundary_knots

spline4_fit <- lm(nox ~ ns(dis, df = 3), data = Boston)
summary(spline4_fit)$df[1] # 4 degrees of freedom in total

ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_vline(aes(xintercept = spline4_knots[1], col = "Knots"), linetype = "dashed") +
  geom_vline(aes(xintercept = spline4_knots[2], col = "Knots"), linetype = "dashed") +
  geom_vline(aes(xintercept = spline4_boundary_knots[1], col = "Boundary Knots"), linetype = "dashed") +
  geom_vline(aes(xintercept = spline4_boundary_knots[2], col = "Boundary Knots"), linetype = "dashed") +
  scale_color_manual(values = c("grey55", "mediumseagreen")) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ ns(x, df = 3)") + # note the identical fit to the cubics
  coord_cartesian(ylim = c(0.3, 0.9)) +
  theme(legend.position = "bottom") +
  labs(title = "Natural Cubic Spline with 2 Knots", col = "")

spline5a_knots <- attr(bs(Boston$dis, df = 3, degree = 2), "knots")
spline5a_knots

spline5a_fit <- lm(nox ~ bs(dis, df = 3, degree = 2), data = Boston)
summary(spline5a_fit)$df[1] # 4 degrees of freedom in total

spline5b_knots <- attr(bs(Boston$dis, df = 3, degree = 1), "knots")
spline5b_knots

spline5a_fit <- lm(nox ~ bs(dis, df = 3, degree = 2), data = Boston)
summary(spline5a_fit)$df[1] # 4 degrees of freedom in total

g1 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_vline(aes(xintercept = spline5a_knots, col = "Knots"), linetype = "dashed") +
  scale_color_manual(values = "mediumseagreen") +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 3, degree = 2)") + # note the identical fit to the cubics
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Quadratic Spline with 1 Knot", col = "")

g2 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_vline(aes(xintercept = spline5b_knots[1], col = "Knots"), linetype = "dashed") +
  geom_vline(aes(xintercept = spline5b_knots[2], col = "Knots"), linetype = "dashed") +
  scale_color_manual(values = "mediumseagreen") +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 3, degree = 1)") + # note the identical fit to the cubics
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "Linear Spline with 2 Knots", col = "")

grid.arrange(g1, g2, nrow = 2)


#(e) Cubic Splines - Training MSE

#Q: Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits and report the resulting RSS. Describe the results obtained.

#A:



MSE <- c()

i <- 1
for (p in 3:12) {
  model_temp <- lm(nox ~ bs(dis, df = p), data = Boston)
  
  MSE[i] <- mean(model_temp$residuals^2)
  i <- i+1
}

data.frame(df = 4:13, MSE = MSE) %>%
  mutate(min_MSE = as.numeric(min(MSE) == MSE)) %>%
  ggplot(aes(x = df, y = MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_MSE))) +
  scale_x_continuous(breaks = seq(4, 13), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.00004)) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Boston Dataset - Cubic Splines",
       subtitle = "Selecting the 'dis' degrees of freedom with training MSE",
       x = "Degrees of Freedom", 
       y = "Training MSE")

attr(bs(Boston$dis, df = 4), "knots")

attr(bs(Boston$dis, df = 5), "knots")

g1 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 3)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 4 (0 knots)")

g2 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 4)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 5 (1 knot)")

g3 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 5)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 6 (2 knots)")

g4 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 6)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 7 (3 knots)")

g5 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 7)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 8 (4 knots)")

g6 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 8)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 9 (5 knots)")

g7 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 9)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 10 (6 knots)")

g8 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 10)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 11 (7 knots)")

g9 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 11)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 12 (8 knots)")

g10 <- ggplot(Boston, aes(x = dis, y = nox)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", formula = "y ~ bs(x, df = 12)") + 
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(title = "df = 13 (9 knots)")
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol = 2)

# (f) Cubic Splines - Tuning Degrees of Freedom (CV MSE)

# Q: Perform cross-validation or another approach in order to select the best degrees of freedom for a regression spline on this data. Describe your results.

# A: I iterate through the same 10 regular cubic spline fits, where I vary the degrees of freedom from 4 (no knots) to 13 (10 knots), using 5 repeats of 10-fold cross-validation to select the best fit.

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = custom_regression_metrics)

CV_MSE <- c()

set.seed(3)

i <- 1
for (p in 3:12) {
  model_temp <- train(y = Boston$nox,
                      x = bs(Boston$dis, df = p),
                      method = "lm",
                      metric = "MSE",
                      trControl = ctrl)
  CV_MSE[i] <- model_temp$results$MSE
  i <- i + 1
}


data.frame(df = 4:13, CV_MSE = CV_MSE) %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = df, y = CV_MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) +
  geom_hline(yintercept = min_poly_MSE_raw) + 
  geom_hline(yintercept = min_poly_MSE_no_outliers) + 
  scale_x_continuous(breaks = seq(4, 13), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.00002)) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Boston Dataset - Cubic Splines",
       subtitle = "Selecting the 'dis' degrees of freedom with cross-validation MSE",
       x = "Degrees of Freedom", 
       y = "Cross-Validation MSE")    


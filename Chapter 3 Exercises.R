# CS5565 Assignment #1 
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 3.7 Applied Problem 8
  
  # Load Auto dataset into Auto object and remove all NA values
  Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
  Auto = na.omit(Auto)
  
  # a) Perform a simple linear regression with horsepower as predictor and mpg as the response
  Auto.fit = lm(mpg ~ horsepower, data = Auto)
  summary(Auto.fit)
  
  # The relationship between the predictor and response is written as mpg = -0.1578 horsepower + 39.9359
  # There is a negative relationship between the predictor and response since the coefficient in front of horsepower
  # is negative. There is a moderately strong relationship between horsepower and mpg since the R^2 statistic is 0.6059.

  # Using the linear fit, predict mpg associated with a horsepower of 98
  predict(Auto.fit, data.frame(horsepower = 98))
  
  # Associated 95 % confidence and prediction intervals given horsepower of 98
  predict(Auto.fit, data.frame(horsepower = 98), interval = "confidence")
  predict(Auto.fit, data.frame(horsepower = 98), interval = "prediction")
  
  # b) Use the response variable mpg and predictor horsepower to generate a scatterplot
  plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
  
  # Generate the least squares regression line with a line width of 3 and red color for visibility
  abline(Auto.fit, lwd= 3, col = "red")
  
  # c) Produce diagnostic plots of the least squares regression fit
  par(mfrow = c(2, 2))
  plot(Auto.fit)
  
  # Findings: The presence of a U-shaped pattern in the residuals plot indicates there is not a linear relationship between
  # the predictor and response variables. 
  
# ISLR 3.7 Applied Problem 9
  # Replace the values in origin with their meanings, and convert the variable into a factor
  Auto$origin[Auto$origin == 1] = "American"
  Auto$origin[Auto$origin == 2] = "European"
  Auto$origin[Auto$origin == 3] = "Japanese"
  Auto$origin = as.factor(Auto$origin)
  
  # a) Produce a scatterplot with all the quantitative variables in the data set
  pairs(Auto[,1:7])
  
  # b) Compute the matrix of correlations between the quantitative variables
  cor(Auto[,1:7])
  
  # c) Perform a multiple linear regression using mpg as the response variable
 mpg.mlr= lm(mpg~ . -name, data = Auto)
 summary(mpg.mlr)
 
  # Figuring out where the coefficient for US origin went
 contrasts(Auto$origin)
 
  # US origin would be 0 for both European and Japanese origin
 
  # Findings: There are several predictor variables with statistically significant relationships with the response variable; namely
  # those with p-values lower than 0.05, which would include displacement, weight, year, and origin. The coefficients for origin indicate 
  # that European and Japanese cars are more fuel efficient by 2.630 and 2.853 miles per gallon on average.
 
  # d) Produce diagnostic plots of the linear regression fit
 par(mfrow = c(2, 2))
 plot(mpg.mlr)
 
  # Findings: There is also a U-shaped curve in the residuals plot indicating that there isn't a linear relationship between the predictors
  # and the response variable. There is a single outlier with high leverage.
   
  # e)
  # Backward selection: Use ^2 fit a linear regression model with all possible second order interaction effects
   mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
   summary(mpg.fit.all.interactions)
   
   # Adjusted R^2 value= 0.8866
   
   # Remove the interaction with the highest p-value, one at a time, then refit model and check R^2 value
   # Interactions are statistically significant if removing them changes the R^2 of the model
   
   # Remove horsepower and originEuropean interaction
   mpg.fit.reduced = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
   summary(mpg.fit.reduced)
   
   # Adjusted R^2 value = 0.8872
   
   # Remove displacement and horsepower interaction
   mpg.fit.reduced = update(mpg.fit.all.interactions, ~ . - displacement:horsepower)
   summary(mpg.fit.reduced)
   # Adjusted R^2 value = 0.8875. Note that R^2 value increases are becoming smaller. 
   
   # We can also use forward selection to add statistically significant interaction effects and see how they effect the adjusted R^2
   
   # For example, the p-value of the cylinders:displacement interaction is close to 0
   summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))
   
   # Adding just the cylinders:displacement interaction variable improved the adjusted R^2 from 0.8205 to 0.8436
   
  # f) Graph scatterplots of some of the variables to determine if there are relationships that could use transformations in the variables
    # to improve the model
   
   par(mfrow = c(2, 2))
   plot(Auto$displacement, Auto$mpg)
   plot(Auto$horsepower, Auto$mpg)
   plot(Auto$weight, Auto$mpg)
   plot(Auto$acceleration, Auto$mpg)
   
   # Try sqrt transform of acceleration
   
   acceleration.linear= lm(mpg ~ acceleration, data = Auto)
   summary(acceleration.linear)
   
   acceleration.sqrt= lm(mpg ~ sqrt(acceleration), data = Auto)
   summary(acceleration.sqrt)
   
   # The sqrt transform improved R^2 of the model, from 0.1771 to 0.1834
   
   # Transform of displacement
   displacement.linear = lm(mpg ~ displacement, data = Auto)
   summary(displacement.linear)
   
   displacement.quadratic = lm(mpg ~ displacement + I(displacement^2), data = Auto)
   summary(displacement.quadratic)
   
   # The R^2 improved from 0.6473 to 0.6872, indicating the quadratic transform was a better fit for displacement.
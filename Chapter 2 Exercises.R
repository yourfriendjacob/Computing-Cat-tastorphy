# CS5565 Assignment #1 
# Group name: Computing Cat-tastorphy
# Members: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng

# ISLR 2.4 Applied Problem 8

  # a) Load data from College.csv into object called college
  
  college = read.csv("College.csv")
  
  # b) View loaded data
  
  # Create column with names of each college (from 1st column of data)
  rownames(college) = college[, 1]
  
  View(college)
  
  # Delete first column where names are also stored
  college = college[,-1]
  
  View(college)
  
  # c) Generate summary of all variables in data set
  summary(college)
  
  # Generate scatterplot of first 10 columns in data
  pairs(college[, 2:10])
  
  # Produce side by side boxplots of Outstate vs Private
  boxplot(college$Outstate ~ college$Private, ylab = "Out of state tuition (in $)", xlab= "Private (Y/N)")
  
  # Create a new variable, Elite, by dividing universities into 2 groups based on whether or not
  # the proportion of students coming from top 10% of high school classes exceeds 50%
  
    # Vector of "no" values matching the number of rows in college data set
    Elite = rep("No", nrow(college))
    
    # Assign "Yes" to values in Elite that correspond to colleges where > 50% of the class comes from top 10% of high school class
    Elite[college$Top10perc > 50] = "Yes"
  
    # Encodes values in Elite as factors
    Elite = as.factor(Elite)  
    
    # Add Elite column to college data 
    college = data.frame(college, Elite)
  
  # Use summary function to view number of elite colleges
    summary(college$Elite)
    
    View(college$Elite)
  
  # Produce side by side boxplots of Outstate vs Elite
  boxplot(college$Outstate ~ college$Elite, ylab = "Out of state tuition (in $)", xlab= "Elite (Y/N)")
 
  # Partitions graph window into multiple graphs; in this case 2 x 2 graphs
  par(mfrow = c(2, 2))
  hist(college$S.F.Ratio, xlab = "Student-Faculty Ratio", main = "Histogram for all colleges")
  hist(college$S.F.Ratio[college$Private == "Yes"], xlab = "Student-Faculty Ratio", main = "Histogram for private schools")
  hist(college$S.F.Ratio[college$Private == "No"], xlab = "Student-Faculty Ratio", main = "Histogram for public schools")
  hist(college$S.F.Ratio[college$Elite == "Yes"], xlab = "Student-Faculty Ratio", main = "Histogram for elite schools")
  
  # Create variable for acceptance rate and add it to college data set
  AcceptPerc = college$Accept / college$Apps * 100
  college = data.frame(college, AcceptPerc)
  
  par(mfrow = c(1, 2))
  plot(college$AcceptPerc, college$Terminal, ylab = "% Faculty with terminal degree", xlab = "Acceptance rate")
  boxplot(college$Terminal ~ college$Elite, xlab = "Elite", ylab = "% Faculty with terminal degree")
  summary(college$AcceptPerc)
  
  # Check data on % of faculty with terminal degrees for non Elite and Elite colleges
  summary(college$Terminal[college$Elite == "Yes"])
  summary(college$Terminal[college$Elite == "No"])
  
  # Check data on acceptance rate for colleges given % of faculty with terminal degrees
  summary(college$AcceptPerc[college$Terminal > 80])
  summary(college$AcceptPerc[college$Terminal <= 80])
  
  # Findings:
  # The median % of faculty with terminal degrees was lower at non elite colleges
  # The median acceptance rate at colleges where 80% or less of faculty had terminal degrees was higher than 
  # at colleges where more than 80% of faculty had terminal degrees
  
# ISLR 2.4 Applied Problem 9
  # Load data from Auto.csv into object called Auto, replace all ? strings with NA
  # header = TRUE: first row of data contains variable names
  Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
  
  # Remove missing values from data
  Auto = na.omit(Auto)
  
  # Check to make sure all NA values have been removed (will only return object if there are no missing values)
  na.fail(Auto)
  
  # a) Determine the class of each variable in data set
  sapply(Auto, class)

  # Qualitative predictors: origin, name
  # Quantitative predictors: mpg, cylinders, displacement, horsepower, weight, acceleration, year
  
  # b) Find range of each quantitative predictor
  range(Auto$mpg)
  range(Auto$cylinders)
  range(Auto$displacement)
  range(Auto$horsepower)
  range(Auto$weight)
  range(Auto$acceleration)
  
  # c) Find mean and st. dev of each quantitative predictor
  colMeans(Auto[,1:7])
  
  # MARGIN = 2 references to applying st. dev function over columns
  apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")
  
  # d) Remove 10th through 85th values (rows 10-85), then apply function to find range, mean, and st. dev of quantitative predictors
  apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
  apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
  apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")
  
  # e) and f)
  
  # Looking at the relationship between origin and mpg, origin and horsepower, and origin and weight
  par(mfrow = c(1, 3))
  boxplot(Auto$mpg ~ Auto$origin, ylab = "Miles per gallon", xlab= "Origin")
  boxplot(Auto$horsepower ~ Auto$origin, ylab = "Horsepower", xlab= "Origin")
  boxplot(Auto$weight ~ Auto$origin, ylab = "Weight", xlab= "Origin")
  
  # Findings: The cars from origin "3" (Japanese) had the highest mpg and also the lowest weight, which makes sense 
  # as lowering the weight improves fuel economy. Origin "1" (US) cars had the lowest mpg but also highest horsepower and weight
  
  par(mfrow = c(2, 2))
  plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
  plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")
  plot(Auto$weight, Auto$mpg, xlab = "Weight", ylab = "Miles per gallon")
  plot(Auto$cylinders, Auto$mpg, xlab = "Number of cylinders", ylab = "Miles per gallon")
  
  # Findings: Weight and (interestingly) horsepower are negatively associate with mpg. There is a weak positive relationship
  # between model year and mpg which suggests newer cars have better fuel economy. High horsepower cars tend to be luxury or sports cars
  # where fuel economy isn't a major consideration. 
  
  # Overall, for quantitative predictors horsepower, weight, and year showed stronger relationships
  # with mpg to serve as good predictors, whereas for qualitative predictors origin could be a good predictor for mpg, since
  # each level of origin had a distinctive range of values for mpg.
  
# ISLR 2.4 Applied Problem 10
  
  # a) Load the Boston data set from the MASS library
  library(MASS)
  
  View(Boston)
  ?Boston
  
  # Retrieves dimensions of Boston data set
  dim(Boston)
  
  # There are 506 rows representing tracts of land in Boston's suburbs. There are 20 columns representing various demographic information 
  # about the tract of land and the housing on it.
  
  # b) Plot some scatterplots of the predictors
  par(mfrow = c(2, 2))
  plot(Boston$indus, Boston$medv, xlab = "Proportion of non-retail business acres", ylab = "Median home value (in $1000s)")
  plot(Boston$age, Boston$medv, xlab = "Proportion of homes built before 1940", ylab = "Median home value (in $1000s)")
  plot(Boston$age, Boston$nox, xlab = "Proportion of homes built before 1940", ylab = "Nitrogen oxide concentration (pp/10 million)")
  plot(Boston$ptratio, Boston$tax, xlab = "Pupil-to-teacher ratio", ylab = "Property tax rate (per $10000)")
  
  # Findings: There was a positive association between the proportion of homes built before 1940 and the nitrogen oxide concentration, suggesting
  # that older homes tend to have higher nitrogen oxide concentrations. There was a weaker negative association between proportion of homes built
  # before 1940 and median home value. There is a notable amount of outliers of areas with larger proportions of older homes but high median home value,
  # suggesting possibly some historic neighborhoods with high property values. 
  
  # c) 
  par(mfrow = c(2, 2))
  plot(Boston$indus, Boston$crim, xlab = "Proportion of non-retail business acres", ylab = "Per capita crime rate")
  plot(Boston$ptratio, Boston$crim, xlab = "Pupil-to-teacher ratio", ylab = "Per capita crime rate")
  plot(Boston$dis, Boston$crim, xlab = "Average distance to 5 employment centers", ylab = "Per capita crime rate")
  plot(Boston$tax, Boston$crim, xlab = "Property tax rate (per $10000)", ylab = "Per capita crime rate")
  
  par(mfrow = c(2, 2))
  plot(Boston$age, Boston$crim, xlab = "Proportion of homes built before 1940", ylab = "Per capita crime rate")
  plot(Boston$lstat, Boston$crim, xlab = "Proportion of lower status residents", ylab = "Per capita crime rate")
  plot(Boston$medv, Boston$crim, xlab = "Median home value", ylab = "Per capita crime rate")
  boxplot(Boston$nox, Boston$crim, xlab = "Proportion of lower status residents", ylab = "Per capita crime rate")

  
  # Findings: The proportion of older homes has a positive relationship with crime rate. Also, median home value is negatively associated
  # with crime rate, which means wealthier neighborhoods predictably tend to have lower crime rates. Also, average distance to 5 employment centers 
  # has a negative relationship with crime rate which suggests that urban centers of employment are close to areas with high crime rates as well.
  
  # d) 
  
  # Create histograms of crime rates, tax rates, and pupil-teacher ratios in Boston
  par(mfrow = c(2, 2))
  hist(Boston$crim, xlab = "Per capita crime rate")
  hist(Boston$tax, xlab = "Property ax rate (per $10000)")
  hist(Boston$ptratio, xlab = "Pupil-teacher ratio")
  
  summary(Boston$crim)
  summary(Boston$tax)
  summary(Boston$ptratio)
  
  # Findings: There seems to be tracts with high crime rates, since the median is only 0.25651 while the highest value is 88.972.
  # Based on the histogram, towards the higher end of pupil-teacher ratio and property tax rate, there are a large number of observations.
  # This indicates there are tracts with much higher than average pupil-teacher ratios and property rates.
  
  # e)
  sum(Boston$chas)
  # 35 tracts bound the Charles River.
  
  # f) The mean pupil-teacher ratio is 18.46 pupils for every teacher.
  
  # g)
  min(Boston$medv)
  Boston[Boston$medv == 5,]
  
  # Findings: The lowest median home value is $5,000 and occurs on 2 tracts. The property tax rate for both tracts is higher than the median
  # and falls on the 3rd quartile. The crime rates for both tracts at 38.3518 and 67.9208 are significantly higher than the median at 0.25651.
  # The pupil-teacher ratio for both tracts is also above the median at 20.2, which falls on the 3rd quartile.
  
  # h)
  sum(Boston$rm > 7)
  sum(Boston$rm > 8)
  
  # There are 64 tracts that average more than 7 rooms per dwelling, and 13 that average more than 8 rooms per dwelling
  summary(Boston[Boston$rm > 8,])

  # Findings: The tracts that average more than 8 rooms per dwelling have a higher median crime rate at 0.52014
  # compared to the population median of 0.25651. The median home value at $44,200 is actually higher than the population median of $21,200.
  # The pupil-teacher ratio median at 17.40 is slightly lower than the population median of 18.46.
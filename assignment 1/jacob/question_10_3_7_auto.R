#Jacob Sauther
#CS5565
#assignment 1, question 9 3.7
#Fall 2022

auto_data = read.csv("C:/Users/Jacob/Documents/GitHub/UMKC/umkc/5565/assignment 1/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
View(auto_data)
dim(auto_data)
auto_data = na.omit(auto_data)

pairs(auto_data[,1:7])

auto_data$origin[auto_data$origin == 1] = 'American'
auto_data$origin[auto_data$origin == 2] = 'European'
auto_data$origin[auto_data$origin == 3] = 'Japanese'
auto_data$origin = as.factor(auto_data$origin)
head(auto_data)

cor(auto_data[, 1:7])
# finds the co-relation between the columns

mpg.fit = lm(mpg ~. -name, data = auto_data)
summary(mpg.fit)
contrasts(auto_data$origin)
# there appears to be some level of relationship between the predictors and response
# Notice that the f statistic is fairly high and the p value very close to 0.
# The responses that appear to be most significant are displacement, weight, year, orginEuropean, and orignJapanese.
# They are most significant due to their P values being very close to 0.
# The coefficient for year suggests that if all other predictors are fixed we should see an increase of mpg by about 0.777 per year.

par(mfrow = c(2,2))
plot(mpg.fit)
# There is a u shape with the residuals which points to non-linearity.
# Many residuals appear indicate some outliers in the data. Two points have exceptionally large leverage as well.

mpg.fit.all.inter = lm(mpg ~ (. - name)^2, data = auto_data)
summary(mpg.fit.all.inter) # r^2: 0.8866
# remove horsepower:origin

mpg.fit.reduce.inter = update(mpg.fit.all.inter, ~ . - horsepower:origin)
summary(mpg.fit.reduce.inter) #r^2:  0.8872
# remove displacement:horsepower

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - displacement:horsepower)
summary(mpg.fit.reduce.inter) #r^2: 0.8875
# remove weight:acceleration

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - weight:acceleration)
summary(mpg.fit.reduce.inter) #r^2: 0.8877 
# remove weight:year

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - weight:year)
summary(mpg.fit.reduce.inter) #r^2: 0.888
# remove cylinders:horsepower

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - cylinders:horsepower)
summary(mpg.fit.reduce.inter) #r^2: 0.8882
# remove cylinders:origin

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - cylinders:origin)
summary(mpg.fit.reduce.inter) #r^2: 0.8879 
# add back cylinders:origin, remove displacement:acceleration

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . + cylinders:origin - displacement:acceleration)
summary(mpg.fit.reduce.inter) #r^2: 0.8884 
# remove cylinders:origin

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - cylinders:origin)
summary(mpg.fit.reduce.inter) #r^2: 0.8882
# remove cylinders:displacement

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - cylinders:displacement)
summary(mpg.fit.reduce.inter) #r^2: 0.8883
# remove cylinders:weight

mpg.fit.reduce.inter = update(mpg.fit.reduce.inter, ~ . - cylinders:weight)
summary(mpg.fit.reduce.inter) #r^2: 0.8886 

# significant interactions (p < 0.001)
#  cylinders:acceleration, displacement:originJapanese, acceleration:originEuropean, acceleration:originJapanese
# somewhat significant interactions (0.001 < p < 0.01)
#  weight:originJapanese
# less influential significant interactions (0.01 < p < 0.5)
#  displacement:year, horsepower:year, year:originEuropean, year:originJapanese
# slightly insignificant interactions (0.5 < p < 0.1)
#  horsepower:acceleration
# insignificant interactions (p > 0.1)
#  displacement:originEuropean, horsepower:weight, weight:originEuropean, cylinders:year 


par(mfrow = c(2,2))
plot(auto_data$displacement, auto_data$mpg)
plot(auto_data$horsepower, auto_data$mpg)
plot(auto_data$weight, auto_data$mpg)
plot(auto_data$acceleration, auto_data$mpg)

# weight
# normal (no transformation)
summary(lm(mpg ~ weight, data = auto_data)) #r^2 0.6918
par(mfrow = c(2,2))
plot(lm(mpg ~ weight, data = auto_data))

# log transformation
summary(lm(mpg ~ log(weight), data = auto_data)) #r^2 0.7119 
par(mfrow = c(2,2))
plot(lm(mpg ~ log(weight), data = auto_data))

# sqrt transformation
summary(lm(mpg ~ sqrt(weight), data = auto_data)) #r^2 0.705 
par(mfrow = c(2,2))
plot(lm(mpg ~ sqrt(weight), data = auto_data))

# ^2 transformation
summary(lm(mpg ~ weight^2, data = auto_data)) #r^2 0.6918
par(mfrow = c(2,2))
plot(lm(mpg ~ weight^2, data = auto_data))

# acceleration
# normal (no transformation)
summary(lm(mpg ~ acceleration, data = auto_data)) #r^2 0.1771 
par(mfrow = c(2,2))
plot(lm(mpg ~ acceleration, data = auto_data))

# log transformation
summary(lm(mpg ~ log(acceleration), data = auto_data)) #r^2 0.1879 
par(mfrow = c(2,2))
plot(lm(mpg ~ log(acceleration), data = auto_data))

# sqrt transformation
summary(lm(mpg ~ sqrt(acceleration), data = auto_data)) #r^2 0.1834
par(mfrow = c(2,2))
plot(lm(mpg ~ sqrt(acceleration), data = auto_data))

# ^2 transformation
summary(lm(mpg ~ acceleration^2, data = auto_data)) #r^2 0.1771
par(mfrow = c(2,2))
plot(lm(mpg ~ acceleration^2, data = auto_data))

# cylinders
# normal (no transformation)
summary(lm(mpg ~ cylinders, data = auto_data)) #r^2 0.6037 
par(mfrow = c(2,2))
plot(lm(mpg ~ cylinders, data = auto_data))

# log transformation
summary(lm(mpg ~ log(cylinders), data = auto_data)) #r^2 0.6024  
par(mfrow = c(2,2))
plot(lm(mpg ~ log(cylinders), data = auto_data))

# sqrt transformation
summary(lm(mpg ~ sqrt(cylinders), data = auto_data)) #r^2 0.6048 
par(mfrow = c(2,2))
plot(lm(mpg ~ sqrt(cylinders), data = auto_data))

# ^2 transformation
summary(lm(mpg ~ cylinders^2, data = auto_data)) #r^2 0.6037
par(mfrow = c(2,2))
plot(lm(mpg ~ cylinders^2, data = auto_data))

# looking at mpg on weight it appears here we have the best transformation with a log transformation.
# Granted this was not that much better fitted than what we had done above, it did seem to achieve a
#  reasonable f statistic and p value as well. Acceleration on mpg appeared to not have much of great
#  r^2. Even transforming it did not bump up the value as much. Cylinders appeared to have slightly higher
#  r^2 but weight was still higher before the transformations.

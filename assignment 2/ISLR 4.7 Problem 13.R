#Jacob Sauther
#CS5565
#assignment 2, question 13, 4.8
#Fall 2022

library(ISLR)  #import ISLR library
library(MASS)  #import MASS library for LDA
library(class) #import class for KNN

head(Weekly)

summary(Weekly) #create a summary of the data
# Looking at the data I can see that it is between 1990 and 2010. The lag values seem to be all at min -18.1950 and at most 12.0260.
# The means for the lag columns appear to be almost the same, but shift a little between each column. The direction column indicates
# that a majority of the rows are going up. It is interesting to me that the 3rd quartile is almost the same across all the lag columns.

pairs(Weekly) #pair the data together on a scatter plot.
# Here I don't see any pattern other than between year and volume. It appears here that there is a relationship where as the years go on
# the volume of trades exponetially increases.

plot(Weekly$Volume, ylab = 'Shares traded (in billions)') #plot volume
# I am seeing the same sort of pattern. I am noticing a large population of points on the plot seem to less than 2 billion shares traded.

cor(Weekly[-9]) #correlation of the columns on each other
# it is tough to nail down correlations between these column. Volume and year appear to have a correlation, but volume has really weak relations
# with everything else. This was identified with the scatter plot. There is a weak relationship between today's returns and all the other columns
# but that relation is very weak.

glm.full = glm(Direction ~ . - Year - Today, data = Weekly, family = 'binomial')
summary(glm.full)
# lag2 appears to have the highest P value of about 2.9%. This is well in a 5% significance threshold. After that Lag1 is there at around 11% P value.


#confusion matrix
glm.full.probs = predict(glm.full, type = 'response')
glm.full.pred = rep('Down', 1089)
glm.full.pred[glm.full.probs > 0.5] = 'Up'
table(glm.full.pred, Weekly$Direction) # get the predictors ready

mean(glm.full.pred == Weekly$Direction) # mean of ~0.5610
# The prediction only got the direction right about 50% of the time. Which is not great. This mean reflects the accuracy of the model.
# Note here that out of 484 down rows, the model predicted 430 of those to be up rows. Similarly out of 605 up rows only 48 of them were predicted as
# down rows.

#logistic regression with Lag2 as only predictor
train = (Weekly$Year < 2009)
glm.fit = glm(Direction ~ Lag2, data = Weekly, subset = train, family = 'binomial')
summary(glm.fit) #try to predict using only Lag2

glm.probs = predict(glm.fit, Weekly[!train, ], type = "response")
glm.pred = rep("Down", dim(Weekly[!train, ])[1])
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly[!train, ]$Direction)

mean(glm.pred == Weekly[!train, ]$Direction) # mean of ~0.625
# The mean here reflects an accuracy of about 62.5%. Which is better than before. Out of 43 down values, 9 were predicted correctly (~20%). Similarly out of 61 up values
# 56 (~91%) were predicted correctly.

mean(Weekly[!train, ]$Direction == "Up") # mean of ~0.5865
# This mean is the accuracy of a row being correctly predicted up. Again not bad, but not much of an improvement.

#LDA modeling
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.fit #use lda to create a model on Direction and Lag2

lda.pred = predict(lda.fit, Weekly[!train, ])
table(lda.pred$class, Weekly[!train, ]$Direction)
mean(lda.pred$class == Weekly[!train, ]$Direction) # mean of ~0.625
# This unfortunately this is about the same as previous prediction. Not much improvement. It has the same values of what predictors failed to predict correctly.

#QDA modeling
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit

qda.pred = predict(qda.fit, Weekly[!train, ])
table(qda.pred$class, Weekly[!train, ]$Direction)

mean(qda.pred$class == Weekly[!train, ]$Direction) #mean of ~0.5865
# This model literally predicted that every week was an up week. Which sure means that all up weeks were predicted correctly, but all down weeks were predicted
# incorrectly. True positive rate is 1 and false positive rate is also 1. Sure it is slightly better than where we started, it is not as great of an improvement.

train.X = data.frame(Weekly[train, ]$Lag2)
test.X = data.frame(Weekly[!train, ]$Lag2)
train.Direction = Weekly[train, ]$Direction #train and test KNN data

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Weekly[!train, ]$Direction) #create KNN K = 1 model

mean(knn.pred == Weekly[!train, ]$Direction) # mean of ~0.5
# With this model we don't get a great model here either. It has a prediction accuracy of 50%, which is more or less just guessing. Out of 43 down predictions
# 21 were predicted correctly (~48%). Out of 61 up records 31 were predicted correctly (~51%). This is the worst accuracy, but has the best rate of not getting
# false positives oddly enough.

# Out of all the models here is a summary of everything so far
# confusion matrix:    accuracy - 56%   true negative rate - 11% true positive rate - 92%  
# logistic regression: accuracy - 62.5% true negative rate - 20% true positive rate - 91%  
# LDA:                 accuracy - 62.5% true negative rate - 20% true positive rate - 91%  
# QDA:                 accuracy - 58.6% true negative rate - 0%  true positive rate - 100% 
# KNN k = 1:           accuracy - 50%   true negative rate - 48% true positive rate - 51%  
# 
# looking at just accuracy the best ones would be LDA or logistic regression. QDA just behind that and KNN k = 1 being at last.
# however it should be noted that most of these except for KNN k = 1 had a bias to predict up weeks more than down weeks. Basically, for most of the models
# it was more likely to predict a week to be up over down most of the time. Depending on the use case you may want to use an overall less accurate model if
# it is predicting down weeks a little more accurately. KNN really is not great for most use cases in my opinion as it would be as good just flipping a coin
# at the start of each week to predict. QDA is honestly not that great of a choice either as it just predicts every week as up. Sure it is "more" accurate than
# KNN, but it is like a broken clock. It is right at least 2 times a day. LDA and logistic regression are the most useful despite them not being not much better
# than the confusion matrix. They are pretty aggressive still with predicting up days so might not be very good for someone not wanting to take risks.




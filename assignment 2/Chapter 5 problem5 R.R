library(ISLR) #download the dataset
head(Default)
set.seed(312)#set a random seed
#Fit a logistic regression model that uses income and balance to predict default
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)
#Split the sample into a training set and a validation set 75/25 respectively
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
#Fit a multiple logistic regression model using only the training observations
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
#Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
#Compute the validation set error, which is the fraction of observations in the validation set that are misclassified
#2.84% of the observations in the validation set errors are misclassified.
mean(glm.preds != Default[-train, "default"])
#Repeat the process in Part 2 three times, using three different splits of the observations into a training set and a validation set. Comment on the results obtained.

#first split
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

#second split
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

#third split
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

#After running the query three time we realized that the error rate remained pretty consistent (0.26%, 0.25%, 0.252%)
#the results varied based on the observations that the system included in the training set and the validation set at each time




#Now consider a logistic regression model that predicts the probability of default using income, balance, and a dummy variable
#for student. Estimate the test error for this model using the validation set approach. Comment on whether or not including a
#dummy variable for student leads to a reduction in the test error
#rate.



#create with student and without student variables
with.student = rep(0, 50)
without.student = rep(0, 50)
#loop 50 times into the dataset at the 75/25 split using the student variable fit and predict
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
#find the difference between the results then measure the error variance
difference = with.student - without.student
errors = data.frame(with.student, without.student, difference)
mean(errors$difference)

#including the student variable in the model doesn't reduce the test error rate.


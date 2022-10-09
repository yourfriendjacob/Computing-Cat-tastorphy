################################ Applied Exercise 1 ###############################################
library(ISLR)
head(Default)
set.seed(312)

### Part 1 ###
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

### Part 2 ###
train = sample(dim(Default)[1], 0.75*dim(Default)[1])

glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])
# 2.84 % of set are misclassified

### Part 3 ###
train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

train = sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

1 - mean(Default[-train, "default"] == "No")

### Part 4 ###
with.student = rep(0, 50)
without.student = rep(0, 50)
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
difference = with.student - without.student
errors = data.frame(with.student, without.student, difference)
mean(errors$difference)

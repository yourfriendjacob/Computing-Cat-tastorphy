#Jacob Sauther
#CS5565
#assignment 4, 8.4 question 8 pg 371
#Fall 2022

library(ISLR2)
library(tree)
library(ggplot2)
library(dplyr)
library(scales)
library(glmnet)
library(randomForest)
library(tibble)
attach(Carseats)

# a: create the train and test split
set.seed(2, sample.kind = "Rounding")

train_index = sample(1:nrow(Carseats), nrow(Carseats) / 2)

train = Carseats[train_index, ] # 200 records
test = Carseats[-train_index, ] # 200 records

# b: fit a regression tree to the training set
tree_mod = tree(Sales ~ ., train)
plot(tree_mod)
text(tree_mod, prett = 0, cex = 0.7)
# from the tree it appears that Shelve Loc and Price are the most important factors for the sale of a carseat.
# though these are not the only factors on why they were purchased.
summary(tree_mod) #This summary indicates 17 terminal nodes
test_pred = predict(tree_mod, test)
mean((test_pred - test$Sales)^2) # find MSE
# the MSE is roughly 4.84. Compared to the MSE of roughly 8.09 this prediction is better with the tree, but we might be able to improve it.
baseline_test_pred = mean(train$Sales)
mean((baseline_test_pred - test$Sales)^2)

# c: cross validate to determine level of complexity. Does pruning improve the tree MSE?
set.seed(3)
cv_tree_mod = cv.tree(tree_mod, K = 10)
data.frame(n_leaves = cv_tree_mod$size,
           CV_RSS = cv_tree_mod$dev) %>%
  mutate(min_CV_RSS = as.numeric(min(CV_RSS) == CV_RSS)) %>%
  ggplot(aes(x = n_leaves, y = CV_RSS)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_CV_RSS))) +
  scale_x_continuous(breaks = seq(1, 17, 2)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Carseats Dataset - Regression Tree",
       subtitle = "Selecting the complexity parameter with cross-validation",
       x = "Terminal Nodes",
       y = "CV RSS")
# the best appears to have no pruning, but let us look at the MSE to determine that. It should be roughly the same since we did not prune it.
pruned_tree_mod = prune.tree(tree_mod, best = 17)
test_pred <- predict(pruned_tree_mod, test)
mean((test_pred - test$Sales)^2)
# we get an MSE of roughly 4.84 again
data.frame(size = cv_tree_mod$size, 
           dev = cv_tree_mod$dev, 
           k = cv_tree_mod$k)
# now here we look at the complexity and we get some strange results. Here k is -Inf when the size is 17. Which indicates us not pruning the tree.
# so our complexity here should be a = 0 despite seeing -Inf.

# d: Use bagging approach to analyze the data. What test MSE is present and use importance() function to determine which vars are most important.
set.seed(1)

bag_tree_mod = randomForest(y = train$Sales,
                            x = train[ , -1],
                            mtry = ncol(train) - 1,
                            importance = T)
test_pred = predict(bag_tree_mod, test)
mean((test_pred - test$Sales)^2)
# MSE is roughly 2.37 now. Which is better than what we had above.
importance(bag_tree_mod) %>%
  as.data.frame() %>%
  rownames_to_column("varname") %>%
  arrange(desc(IncNodePurity))
# we have the following vars in importance:
# Price
# ShelveLoc
# CompPrice
# Age
# Advertising
# Income
# Population
# Education
# Urban
# US

# e: Use random forests to analyze the data. Use importance() to determine most important vars.
test_MSE = c()

i = 1
for (Mtry in 1:10) {
  set.seed(1)
  
  rf_temp = randomForest(y = train$Sales,
                         x = train[ , -1],
                         mtry = Mtry,
                         importance = T)
  test_pred = predict(rf_temp, test)
  test_MSE[i] = mean((test_pred - test$Sales)^2)
  i = i + 1
}

data.frame(mtry = 1:10, test_MSE = test_MSE) %>%
  mutate(min_test_MSE = as.numeric(min(test_MSE) == test_MSE)) %>%
  ggplot(aes(x = mtry, y = test_MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_test_MSE))) +
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Carseats Dataset - Random Forests",
       subtitle = "Selecting 'mtry' using the test MSE",
       x = "mtry",
       y = "Test MSE")

# since the seed was the same the results here for MSE and variable importance are the same as the bagged model]
tail(test_MSE, 1) #MSE of about 2.37
importance(rf_temp) %>%
  as.data.frame() %>%
  rownames_to_column("varname") %>%
  arrange(desc(IncNodePurity))
# same order of importance as in d as well.
# the graphs here also look identical. It does not appear to change the result of what we are seeing here vs what we saw in the bagged model.
# I am sure that if we had a differnt seed on each we would see different values. But as long as they are the same it does appear that they both get the same
# result.
# Assignment 1
# Group: Computing Cat-tastorphy
# Member: Sehtab Hossain, Michelle Lu, Salam Othman, Jacob Sauther, Feng Zheng
# University of Missouri Kansas City
# CS5565-0001

# read csv file
college = read.csv("College.csv")

# given each row named as college
row.names(college) = college[, 1]

# delete the first column college name
college = college[,-1]
head(college)

# summary
summary(college)

# pairs
pairs(college[, 2:10])

# boxplot Private versus Outstate
boxplot(college$Outstate~college$Private, data = college, xlab = "Private", ylab = "Out-of-state Tuition ($)")

# add an Elite cloumn
Elite = rep("No", nrow(college))
Elite[college$Top10per>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

# summary
summary(college$Elite)

# boxplot Elite versus Outstate
boxplot(college$Outstate~college$Elite, data = college, xlab = "Elite", ylab = "Out-of-state Tuition ($)")

# show mulitiple plot at once 
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

par(mfrow = c(1,2))
boxplot(college$PhD~college$Elite, data = college, xlab = "Elite", ylab = "Percent of faculty with Ph.D.’s")
boxplot(college$PhD~college$Private, data = college, xlab = "Private", ylab = "Percent of faculty with Ph.D.’s")

# From my obervation, a Elite school have higher mean percentage of faculty with Ph.D
# In other hand, Public School tend to have higher mean percentage of faculty with Ph.D


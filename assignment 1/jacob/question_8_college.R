#Jacob Sauther
#CS5565
#assignment 1, question 8
#Fall 2022

#load csv file. Want headers. Want strings to be converted to factors. 1 No 2 Yes
college = read.csv("C:/Users/Jacob/Documents/GitHub/UMKC/umkc/5565/assignment 1/College.csv", header = TRUE, stringsAsFactors = TRUE)

#get rownames
rownames(college) = college[, 1]
#trim off the college names
college = college[, -1]
head(college)
#see data summary
summary(college)

#pairs for the first 10 columns
pairs(college[, 1:10])

#plot outstate against private


#create elite column and append it to college
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = 'Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)

#view the college data and plot
summary(college)


par(mfrow = c(2, 4))
hist(college$Apps)
hist(college$Enroll)
hist(college$PhD)
hist(college$Books)
hist(college$Top25perc)
hist(college$Top10perc)
plot(college$Outstate, college$Elite)
plot(college$Outstate, college$Private)

# It appears that enrollment and applications have a similar pattern.
# private colleges appear to have the most out state students. This seems expected.
# It is noticeable that elite schools seem to have much more out state students as well.
# It appears that there is a high frequency of students getting a Ph.D

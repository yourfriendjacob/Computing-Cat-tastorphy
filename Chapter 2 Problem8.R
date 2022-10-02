# Read the file
college= read.csv("college.csv", header = TRUE)

#run the view function on the dataset
#created a new column to duplicate the first column
rownames(college)= college[, 1]
View(college)
#deleted column1
college = college[, -1]
View(college)
head(college)

#use the summary function
summary(college)

#change the dataset to numeric then run the pairs function
college[,1] = as.numeric(factor(college[,1]))
pairs(college[,1:10])
 
#Use the plot() function to produce side-by-side boxplots of Outstate versus Private
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

#create the Elite column to show the top10 over 50%
Elite = rep("No", nrow(college))
Elite[college$Top10per>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

#use the summary function to show the Elite column
summary(college$Elite)

#Now use the plot() function to produce side-by-side boxplots of Outstate versus Elite
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

#show plots for the number of applicants per the different types of schools
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "2"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "1"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

#show plots for the contributions of alumni to donations to the various schools
#donation percentages are higher for private and elite alumni than the regular schools, which indicates that the alumni from Elite 
#and private schools end up donating more after graduation
par(mfrow = c(2,2))
hist(college$perc.alumni, xlab= "Percent of Alumni that donate", main= "All Alumni that donate")
hist(college$perc.alumni[college$Private =="2"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Private schools")
hist(college$perc.alumni[college$Private =="1"], xlab= "Percent of Alumni that donate", main= "Alumni donating in non-Private schools")
hist(college$perc.alumni[college$Elite =="Yes"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Elite schools")


#show plots and histagrams for the student/faculty ratio
#There are less students per faculty in the Elite and Private schools
plot(college$Private, college$S.F.Ratio, xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="1"], college$S.F.Ratio[college$Private =="1"], xlab = "non-Private Schools", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="2"], college$S.F.Ratio[college$Private =="2"], xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Elite =="Yes"], college$S.F.Ratio[college$Elite =="Yes"], xlab = "Elite", ylab = "S.F.Ratio")


par(mfrow = c(2,2))
hist(college$S.F.Ratio, xlab= "Student/faculty ratio", main= " Overall Student/faculty ratio")
hist(college$S.F.Ratio[college$Private =="2"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Private schools")
hist(college$S.F.Ratio[college$Private =="1"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in non-Private schools")
hist(college$S.F.Ratio[college$Elite =="Yes"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Elite schools")


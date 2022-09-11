college= read.csv("college.csv", header = TRUE)

rownames(college)= college[, 1]
View(college)
college = college[, -1]
View(college)
head(college)


summary(college)

college[,1] = as.numeric(factor(college[,1]))
pairs(college[,1:10])
 
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

Elite = rep("No", nrow(college))
Elite[college$Top10per>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)


summary(college$Elite)

plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "2"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "1"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

#donation percentages- higher % of private and elite alumni donating
par(mfrow = c(2,2))
hist(college$perc.alumni, xlab= "Percent of Alumni that donate", main= "All Alumni that donate")
hist(college$perc.alumni[college$Private =="2"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Private schools")
hist(college$perc.alumni[college$Private =="1"], xlab= "Percent of Alumni that donate", main= "Alumni donating in non-Private schools")
hist(college$perc.alumni[college$Elite =="Yes"], xlab= "Percent of Alumni that donate", main= "Alumni donating in Elite schools")



plot(college$Private, college$S.F.Ratio, xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="1"], college$S.F.Ratio[college$Private =="1"], xlab = "non-Private Schools", ylab = "S.F.Ratio")
plot(college$Private[college$Private =="2"], college$S.F.Ratio[college$Private =="2"], xlab = "Private", ylab = "S.F.Ratio")
plot(college$Private[college$Elite =="Yes"], college$S.F.Ratio[college$Elite =="Yes"], xlab = "Elite", ylab = "S.F.Ratio")


par(mfrow = c(2,2))
hist(college$S.F.Ratio, xlab= "Student/faculty ratio", main= " Overall Student/faculty ratio")
hist(college$S.F.Ratio[college$Private =="2"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Private schools")
hist(college$S.F.Ratio[college$Private =="1"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in non-Private schools")
hist(college$S.F.Ratio[college$Elite =="Yes"], xlab= "Student/faculty ratio", main= "Student/faculty ratio in Elite schools")


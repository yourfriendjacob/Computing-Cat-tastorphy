################################## Applied Exercise 1 ####################################################################
### Part-1 ###
college = read.csv("College.csv", header = TRUE)
View(college)

### Part-2 ###
rownames(college) = college[, 1]
fix(college)
head(college)

# eliminate first column
college = college[, -1]
fix(college)
head(college)

### Part-3.1 ###
summary(college)

### Part-3.2 ###
# produce a scatterplot

#The pairs() function requires numeric columns of a matrix or data frame as input. 
#However, your first column is not numeric but of character type. We can fix this easily:
college[,1] = as.numeric(factor(college[,1]))
pairs(college[, 1:10])

### Part-3.3 ###
plot(college$Private, college$Outstate, xlab = "Private", ylab= "Out-of-state tuition (dollars)")

### Part-3.4 ###
# creating a variable "Elite" by binning Top10perc
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)

plot(college$Elite, college$Outstate, xlab = "Elite", ylab= "Out-of-state tuition (dollars)")

### Part-3.5 ###
# histogram
par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of Applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == TRUE], xlab = "Number of Applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == FALSE], xlab = "Number of Applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == TRUE], xlab = "Number of Applicants", main = "Histogram for Elite schools")

par(mfrow = c(2, 2))
hist(college$Expend, xlab = "Instructional expenditure per student (dollars)", main = "Histogram for all colleges")
hist(college$Expend[college$Private == TRUE], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for private schools")
hist(college$Expend[college$Private == FALSE], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for public schools")
hist(college$Expend[college$Elite == TRUE], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
hist(college$S.F.Ratio, xlab = "Student-Faculty Ratio", main = "Histogram for all colleges")
hist(college$S.F.Ratio[college$Private == TRUE], xlab = "Student-Faculty Ratio", main = "Histogram for private schools")
hist(college$S.F.Ratio[college$Private == FALSE], xlab = "Student-Faculty Ratio", main = "Histogram for public schools")
hist(college$S.F.Ratio[college$Elite == TRUE], xlab = "Student-Faculty Ratio", main = "Histogram for elite schools")

### Part-3.6 ###
NonTuitionCosts = college$Room.Board + college$Books + college$Personal
college = data.frame(college, NonTuitionCosts)
par(mfrow = c(1, 2))
plot(college$Private, college$NonTuitionCosts, xlab = "Private", ylab = "Total non-tuition costs per year (dollars)")
plot(college$Elite, college$NonTuitionCosts, xlab = "Elite", ylab = "Total non-tuition costs per year (dollars)")

AcceptPerc = college$Accept / college$Apps * 100
college = data.frame(college, AcceptPerc)
par(mfrow = c(1, 2))
plot(college$Private, college$AcceptPerc, xlab = "Private", ylab = "Acceptance Rate")
plot(college$Elite, college$AcceptPerc, xlab = "Elite", ylab = "Acceptance Rate")

summary(college$AcceptPerc[college$Private == TRUE])

summary(college$AcceptPerc[college$Private == FALSE])

summary(college$AcceptPerc[college$Elite == TRUE])

summary(college$AcceptPerc[college$Elite == FALSE])

par(mfrow = c(2, 2))
hist(college$perc.alumni, xlab = "Percent of alumni who donate", main = "Histogram for all colleges")
hist(college$perc.alumni[college$Private == TRUE], xlab = "Percent of alumni who donate", main = "Histogram for private schools")
hist(college$perc.alumni[college$Private == FALSE], xlab = "Percent of alumni who donate", main = "Histogram for public schools")
hist(college$perc.alumni[college$Elite == TRUE], xlab = "Percent of alumni who donate", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
plot(college$PhD, college$Grad.Rate, xlab = "Number of faculty with PhDs", ylab = "Graduation Rate")
plot(college$Terminal, college$Grad.Rate, xlab = "Number of faculty with terminal degrees", ylab = "Graduation Rate")
plot(college$S.F.Ratio, college$Grad.Rate, xlab = "Student-faculty ratio", ylab = "Graduation Rate")
plot(college$Expend, college$Grad.Rate, xlab = "Instructional expenditure per student (dollars)", ylab = "Graduation Rate")

################# Applied Exercise 2 ###############################################################################################
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)

### Part-1 ###
head(Auto)

### Part-2 ###
?range
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

### Part-3 ###
colMeans(Auto[, 1:7])
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")

### Part-4 ###
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")

### Part-5 ###
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")

par(mfrow = c(2, 2))
plot(Auto$year, Auto$acceleration, xlab = "Model Year", ylab = "0 to 60mph time (seconds)")
plot(Auto$year, Auto$displacement, xlab = "Model Year", ylab = "Engine displacement (cubic inches)")
plot(Auto$year, Auto$weight, xlab = "Model Year", ylab = "Car weight (pounds)")
plot(Auto$year, Auto$horsepower, xlab = "Model Year", ylab = "Horsepower")



par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")

par(mfrow = c(2, 1))
plot(Auto$weight, Auto$horsepower, xlab = "Car weight (pounds)", ylab = "Horsepower")
plot(Auto$weight, Auto$displacement, xlab = "Car weight (pounds)", ylab = "Engine displacement (cubic inches)")

### Part-6 ###
Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")


######################## Applied Exercise 3 ###################################################################################

### Part 1 ###
library(MASS)
head(Boston)
?Boston
Boston_corrected = read.csv("boston_corrected.csv", header = TRUE)
head(Boston_corrected)

### Part 2 ###
dim(Boston_corrected)

par(mfrow = c(2, 2))
plot(Boston_corrected$AGE, Boston_corrected$CMEDV, xlab = "Percent of units built prior to 1940", ylab = "Median home value in $1000s")
plot(Boston_corrected$LSTAT, Boston_corrected$CMEDV, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(Boston_corrected$CMEDV, Boston_corrected$PTRATIO, xlab = "Median home value in $1000s", ylab = "Pupil-teacher ratio")
plot(as.factor(Boston_corrected$CHAS), Boston_corrected$CMEDV, xlab = "Borders Charles River", ylab = "Median home value in $1000s")

par(mfrow = c(2, 2))
plot(Boston_corrected$CMEDV, Boston_corrected$NOX, xlab = "Median home value in $1000s", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$INDUS, Boston_corrected$NOX, xlab = "Percent of non-retail business acres", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$CMEDV, Boston_corrected$B, xlab = "Median home value in $1000s", ylab = "1000(Proportion of black residents - 0.63)^2")
plot(Boston_corrected$DIS, Boston_corrected$CMEDV, xlab = "Weighted distance to Boston employment centers", ylab = "Median home value in $1000s")

### Part 3 ###
par(mfrow = c(2, 2))
plot(Boston_corrected$B, Boston_corrected$CRIM, xlab = "1000(Proportion of black residents - 0.63)^2", ylab = "Per capita crime rate")
plot(Boston_corrected$LSTAT, Boston_corrected$CRIM, xlab = "Percent of lower status residents", ylab = "Per capita crime rate")
plot(Boston_corrected$CMEDV, Boston_corrected$CRIM, xlab = "Median home value in $1000s", ylab = "Per capita crime rate")
plot(Boston_corrected$DIS, Boston_corrected$CRIM, xlab = "Weighted distance to Boston employment centers", ylab = "Per capita crime rate")

### Part 4 ###
par(mfrow = c(2, 2))
hist(Boston_corrected$CRIM, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston_corrected$TAX, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston_corrected$PTRATIO, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")

summary(Boston_corrected[, c(8, 17, 18)])

### Part 5 ###
sum(Boston_corrected$CHAS)

### Part 6 ###
summary(Boston_corrected$PTRATIO)

### Part 7 ###
min(Boston_corrected$CMEDV)

Boston_corrected[Boston_corrected$CMEDV == 5, ]

summary(Boston_corrected[, c(8:10, 12:20)])

### Part 8 ###
sum(Boston_corrected$RM > 7)

sum(Boston_corrected$RM > 8)

Boston_corrected[Boston_corrected$RM > 8, ]

summary(Boston_corrected[Boston_corrected$RM > 8, c(7:10, 12:20)])




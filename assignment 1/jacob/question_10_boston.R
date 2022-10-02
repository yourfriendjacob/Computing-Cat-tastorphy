#Jacob Sauther
#CS5565
#assignment 1, question 10
#Fall 2022

library(ISLR2)

?Boston
#There are 13 columns with 506 rows of data.
#crim - per capita crime rate
#zn - proportion of residential land zoned for lots over 25,000 sq.ft
#inds - proportion of non-retail business acres per town
#chas - Charles River dummy variable. if bound by river, then 1. Otherwise 0
#nox - nitrogen oxides concentration ppm
#rm - average rooms per dwelling
#age - proportion of owner-occupied units build prior to 1940
#dis - weighted mean of distances to five Boston employment centers
#rad - index of accessibility to radial highways
#tax - full-value property-tax rate per $10,000
#ptratio - pupil-teacher ratio by town
#lstat - lower status of the population
#medv - median value of owner-occupied homes in $1000s

pairs(Boston)
plot(Boston$crim, Boston$inds)
# It looks like this data indicates that the areas with older houses have higher crime rates
# Also houses with higher crime rate have roughly 6 rooms. Though it should be noted it looks like most houses have around 6 rooms.
# The crime rate also appears to spike in somewhat industrious areas.
# The median value of homes increases as there are more rooms, which is expected.
# It looks like the more nitrogen oxide concentration the less the value of the home.

# there are a few predictors that appear to be associated with per capita crime rate.
# age, dis, lstat, ptratio, and medv
# The older the house the more crime appears to occur there, The closer the house is to employment centers the higher the crime,
# The higher the lstat the more crime that appears to occur. The less the median value of the homes the more crime appears to occur.
# It seems that higher pupil teacher ratio also means higher crime rates.

plot(Boston$tax, Boston$zn)
plot(Boston$crim, Boston$zn)
plot(Boston$ptratio, Boston$zn)
# lower zn appears to indicate higher crime.
# no relationship appears to be present between crime and zn
# there does not appear to be a relationship between zn and ptratio

plot(Boston$chas, Boston$zn)
sum(Boston$chas)
# most of the population do not appear to live near the Charles river.
# 35 appear to be near the Charles river

median(Boston$ptratio)
# median ratio appears to be 19.05

min(Boston$medv)
# 5 is the lowest median value of owner-occupied homes

Boston[Boston$medv == 5,]
# these two rows have roughly the same values except for lstat, dis, rm, and crim.
# they have above average crime.

sum(Boston$rm > 7)
# 64 average more than 7 rooms per dwelling

sum(Boston$rm > 8)
# 13 average more than 8 rooms per dwelling

Boston[Boston$rm > 8, ]
summary(Boston[Boston$rm > 8, ])
# these areas appear to be ones with very little crime, low nox, higher tax, and fairly low dis. Buildings in this area also appear to somewhat old. Tending to be roughly 71 years.


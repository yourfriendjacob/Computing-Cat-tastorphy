#Jacob Sauther
#CS5565
#assignment 2, question 13, 4.8
#Fall 2022

library(ISLR) #import ISLR library

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

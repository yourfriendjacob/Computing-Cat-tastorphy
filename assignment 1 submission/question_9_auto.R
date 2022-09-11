#Jacob Sauther
#CS5565
#assignment 1, question 9
#Fall 2022

#load auto data
auto_data = read.csv("C:/Users/Jacob/Documents/GitHub/UMKC/umkc/5565/assignment 1/Auto.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "?")
View(auto_data)
dim(auto_data)
auto_data = na.omit(auto_data)

#--- quantitative predictors
# * mpg
# * cylinders
# * displacement
# * weight
# * acceleration
# * year
# * horsepower

#--- qualitative predictors
# * name
# * origin


mpg_range = range(auto_data$mpg)          # range 9 - 46.6
cyl_range = range(auto_data$cylinders)    # range 3 - 8
dis_range = range(auto_data$displacement) # range 68 - 455
wgt_range = range(auto_data$weight)       # range 1613 - 5140
acc_range = range(auto_data$acceleration) # range 8 - 24.8
yar_range = range(auto_data$year)         # range 70 - 82
hrp_range = range(auto_data$horsepower)   # range 46 - 230

mpg_mean = mean(auto_data$mpg)            # mean ~23.446
cyl_mean = mean(auto_data$cylinders)      # mean ~5.419
dis_mean = mean(auto_data$displacement)   # mean ~193.412
wgt_mean = mean(auto_data$weight)         # mean ~2977.584
acc_mean = mean(auto_data$acceleration)   # mean ~15.541
yar_mean = mean(auto_data$year)           # mean ~75.980
hrp_mean = mean(auto_data$horsepower)     # mean ~104.469

mpg_sd = sd(auto_data$mpg)                # sd ~7.805
cly_sd = sd(auto_data$cylinders)          # sd ~1.706
dis_sd = sd(auto_data$displacement)       # sd ~104.644
wgt_sd = sd(auto_data$weight)             # sd ~849.403
acc_sd = sd(auto_data$acceleration)       # sd ~2.759
yar_sd = sd(auto_data$year)               # sd ~3.684
hrp_sd = sd(auto_data$horsepower)         # sd ~38.450


#range
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "range")
# mpg          11.0 - 46.6
# cylinders    3 - 8
# displacement 68 - 455
# horsepower   46 - 230
# weight       1649 - 4997
# acceleration 8.5 - 24.8
# year         70 - 82

#mean
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
# mpg          24.404
# cylinders    5.373
# displacement 187.241
# horsepower   100.722
# weight       2935.972
# acceleration 15.727
# year         77.146

#standard deviation
apply(auto_data[-(10:85), 1:7], MARGIN = 2, FUN = "sd")
# mpg          7.867
# cylinders    1.654
# displacement 99.678
# horsepower   35.709
# weight       811.300
# acceleration 2.694
# year         3.106

par(mfrow = c(2, 4))
plot(auto_data$mpg, auto_data$weight)
plot(auto_data$mpg, auto_data$cylinders)
plot(auto_data$acceleration, auto_data$horsepower)
plot(auto_data$acceleration, auto_data$cylinders)
plot(auto_data$mpg, auto_data$year)
plot(auto_data$horsepower, auto_data$year)
plot(auto_data$horsepower, auto_data$displacement)
plot(auto_data$mpg, auto_data$horsepower)
# it looks like mpg improves as a car gets lighter. The number of cylinders also seem to make an impact too.
# I see similar results with mpg on horsepower.
# it is kinda interesting to see that more horsepower appears to indicate less acceleration
# better acceleration appears to co-related with less cylinders
# higher displacement appears to mean higher horsepower

#
pairs(auto_data)

#Looking at the pairs for the data I think cylinders, displacement, horsepower, and weight. Could be used to predict the mpg of a car.
#It appears between all the predictors these seem to have a high impact on the mpg. Acceleration may also help, but judging from its relationship with
# horsepower it would seem that you would not need to have both to make a prediction on mpg.


# Feng Zheng

# library ISLR
library(ISLR)
library(boot)
set.seed(312)

# estimated standard errors using glm() and summary()
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

# declare function
boot.fn = function(data, index){
  coefs = coef(glm(default ~ income + balance, data = data, subset = index, 
                   family = "binomial"))[c("income", "balance")]
  return(coefs)
}

# call the function
boot(Default, boot.fn, 1000)

# The standard errors by the bootstrap is quite close to using glm() function.
# This suggests that the data satisfies the underlying assumptions of a logistic regression model

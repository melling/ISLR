# Loading libraries
library(MASS)
library(ISLR)

###################
### QUESTION #8 ###
###################

# Viewing the name of the columns
names(Auto)

# Creating a SLR
auto.lm = lm(mpg~horsepower, data=Auto)

# Viewing statistics of least squares lines
summary(auto.lm)

# i. (Answers obtained using summary(auto.lm))
# There is a relationship between horsepower (predictor) and  mpg (response) 
# because the p-value is extremely below 0.05, which means that chances that 
# this relationship occurred, when there is no relationship at all , is extremely slim,
# therefore there has to be a relationship

# ii. (Answers obtained using summary(auto.lm))
# The relationship is strong, about 60%, because the R^2 = .6059. This statistic 
# measures the proportion of variability in response that can be explained using the
# predictor.

# iii. (Answers obtained using summary(auto.lm))
# The relationship between mpg and horsepower has a negative relationship because the 
# coefficient of horsepower (predictor) is negative

# iv.
predict(auto.lm, data.frame(horsepower=c(98)), interval="prediction")

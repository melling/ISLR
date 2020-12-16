# Loading libraries
library(MASS)
library(ISLR)

# (a) 
pairs(Auto)

# (b)
cor(Auto[,!colnames(Auto) %in% c("name")])

# (c)
# Running a MLR on all predictors except for name
auto.mlr = lm(mpg~.-name, data=Auto)
summary(auto.mlr)

# i.
# There are multiple predictors that have relationship with the response 
# because their associated p-value is significant

# ii.
# The predictors: displacement, weight, year, and origin have a 
# statistically significant relationship.

# iii.
# The coefficient of year suggests that every 4 years, the mpg goes up by 3 

# (d)
par(mfrow=c(2,2))
plot(auto.mlr)

# Non-Linearity: The residual plot shows that there is a U-shape pattern in the residuals 
# which might indicate that the data is non-linear.

# Non-constant Variance: The residual plot also shows that the variance is not constant. There 
# is a funnel shape appearing at the end which indicates heteroscedasticity (non-constant variance)

# Outliers: There seems to not be any outliers because in the  Scale-Location, all values are within 
# the range of [-2,2]. It will only be an outlier if standardized residual is outside the range of 
# [-3, 3].

# High Leverage Points: Based on the Residuals vs. Leverage graph, there is no observations that
# provides a high leverage

# (e) ####

names(Auto)
interact.fit = lm(mpg~.-name+horsepower*displacement, data=Auto)
origin.hp = lm(mpg~.-name+horsepower*origin, data=Auto)
summary(origin.hp)
# Statistically Significant Interaction Terms:
# displacement and horsepower
# horsepower and origin
inter.fit = lm(mpg~.-name+horsepower:origin+horsepower:weight+horsepower:displacement, data=Auto)
summary(inter.fit)
# Adding more interactions, decreases the significance of previous significant values

# (f) ####
summary(lm(mpg~.-name+log(acceleration), data=Auto))
# log(acceleration) is still very significant but less significant than acceleration

summary(lm(mpg~.-name+log(horsepower), data=Auto))
# log(horsepower) is more significant than horsepower

summary(lm(mpg~.-name+I(horsepower^2), data=Auto))
# Squaring horsepower doesnt change the significance

summary(lm(mpg~.-name+I(weight^2), data=Auto))
# Squaring the weights doesnt change significance
lm.fit = lm(mpg~.-name+I(cylinders^2), data=Auto)
plot(lm.fit)
summary(lm(mpg~.-name+I(cylinders^2), data=Auto))
# Squaring the cylinders makes cylinders and horsepower significant variables
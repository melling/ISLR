# Loading libraries
library(MASS)
library(ISLR)

# (a) 
pairs(Auto)
'Horsepower and weight is highly correlated. (RED BOX)
 Displacement and weight are highly correlated. (GREEN BOX)
 MPG and acceleration seems like a non linear relationship (BLUE BOX)
'


# (b)
cor(Auto[,!colnames(Auto) %in% c("name")])
' Numerically view the correlation
'

# (c)
# Running a MLR on all predictors except for name
auto.mlr = lm(mpg~.-name, data=Auto)
summary(auto.mlr)

# i.
# There are multiple predictors that have relationship with the response 
# because their associated p-value is significant. The p-value tells
# us the probability that the coefficient will take a value of 0. The
# typical threshold for p-value is 0.05. If the probability is below 
# 0.05, then that means chances that it will be 0 is very slim.

# ii.
# The predictors: displacement, weight, year, and origin have a 
# statistically significant relationship.

# iii.
# The coefficient of year is 0.7507 which is about 3/4. This tells us 
# the relationship between year and MPG. It suggests that every 3 years,
# the mpg goes up by 4. 

# (d)
par(mfrow=c(2,2))
plot(auto.mlr)

# Non-Linearity: The residual plot shows that there is a U-shape pattern in the residuals 
# which might indicate that the data is non-linear.

# Non-constant Variance: The residual plot also shows that the variance is not constant. There 
# is a funnel shape appearing at the end which indicates heteroscedasticity (non-constant variance)

# Normal Q-Q Plot shows that the residuals are normally distributed if 
# the observations line up on the dashed line. In this case majority of 
# the obeervations lie on the line except for 323, 327, 326.

# Outliers: There seems to not be any outliers because in the  Scale-Location, all values are within 
# the range of [-2,2]. It will only be an outlier if studentized residual is outside the range of 
# [-3, 3].

# High Leverage Points: Based on the Residuals vs. Leverage graph, there is no observations that
# provides a high leverage. To determine if observations contains high leverage, 
# we will have to look to see if there are any points above the red dotted line.
# If there is then that observation has high leverage. In this case, there 
# are no high leverage observations.

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
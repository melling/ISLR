library(ISLR)

set.seed(0)
x = rnorm(100)
y = 2 * x + rnorm(100)

# (a) ####

# Fitting y onto x without intercept
lm.fit.no.intercept = lm(y ~ 0 + x)
summary(lm.fit.no.intercept)

# coeficient: 2.1374
# Standard Error: 0.1092
# t-statistic: 19.58
# p-value: 2e-16

# Since the x is the only predictor, it is highly significant. When x goes up one 
# unit, y goes up two units.

# (b) ####

# Fitting x onto y without intercept
lm.fit.x.y = lm(x ~ 0 + y)
summary(lm.fit.x.y)

# coeficient: 0,37185
# Standard Error: 0.01899
# t-statistic: 19.58
# p-value: 2e-16

# It seems that everytime y goes up by about 100 units, x goes up by about 37 units

# (c) ####

# (a) seems to have a higher standard error and coefficient than (b). However their
# t-statistic and p-value are the same.

# (d) ####



# (e) ####

# The derived equation for t-statistic shows that the x values and y values are only 
# being mulitplied. Since multiplication has a cumulative property, switching the 
# values for x and y will not effect the result.

# (f) ####

# Checking the t-statistic for both models and seeing that it is same for both
summary(lm.fit.no.intercept)
summary(lm.fit.x.y)
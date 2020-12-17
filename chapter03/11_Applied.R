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
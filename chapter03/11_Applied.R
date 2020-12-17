library(ISLR)

set.seed(0)
x = rnorm(100)
y = 2 * x + rnorm(100)

# (a)
lm.fit = lm(y ~ 0 + x)
summary(lm.fit)
# coeficient: 2.1374
# Standard Error: 0.1092
# t-statistic: 19.58
# p-value: -2e-16

# Since the x is the only predictor, it is highly significant. When x goes up one 
# unit, y goes up two units.
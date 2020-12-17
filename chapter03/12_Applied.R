library(ISLR)

# (a) ####

# The coefficent estimate for regression of X onto Y will be the same for coefficent 
# estimate for regression of Y onto X when the there is no irreducible error and 
# there is a perfect linear relationship between x and y (y=x).

# Creating arbitrary data
set.seed(0)
x = rnorm(100)

# Equation Y onto X
y = 2 * x + rnorm(100)

# Fitting y onto x without intercept
lm.fit.no.intercept = lm(y ~ 0 + x)
summary(lm.fit.no.intercept)

# Fitting x onto y without intercept
lm.fit.x.y = lm(x ~ 0 + y)
summary(lm.fit.x.y)

# (c)

# Equation
y = x

# Fitting y onto x
lm.fit.y.x = lm(y ~ 0 + x)
summary(lm.fit.y.x)

# Fitting x onto y
lm.fit.x.y = lm(x ~ 0 + y)
summary(lm.fit.x.y)



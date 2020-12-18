# (a) ####

# Setting the seed to keep random values consistent
set.seed(1)

# Creating a vector with values drawn from a random distribution
x = rnorm(100)

# (b) ####

# Creating a vector with values drawn from a random distribution with mean 0, 
# standard deviation 0.25
eps = rnorm(100, 0, 0.25)

# (c) ####

# Creating the model
Y = -1 + 0.5*x + eps

# The length of vector y is 100 
# B0 = -1
# B1 = 0.5

# (d) ####

plot(x, Y)

# There is a linear relationship 

# (e) ####

lm.fit = lm(Y~x)
summary(lm.fit)

# The coefficients for estimated model are off by a few decimals

# (f) ####

plot(x, Y)
abline(lm.fit, col='red')
abline(-1, 0.5, col="blue")
legend(-1, c("Predicted", "Actual"), col=1:2, lwd=2)

# (g) ####

# Fitting a model introducing a quadratic term
lm.poly.fit = lm(Y~x+I(x^2))
summary(lm.poly.fit)

# Adding the quadratic term does NOT improve the model much (about 0.002). Also 
# quadratic term is not significant according to t-statistic.

# (h) ####

# Generating data randomly for x from normal distribution
x.less = rnorm(100)

# Generating random error terms with small standard deviation (0.05) from normal 
# distribution
eps.less = rnorm(100, 0, 0.05)

# Creating the Y value given x and eps
Y.less = -1 + 0.5*x.less + eps.less

# Fitting a least square regression
lm.less.fit = lm(Y.less~x.less)

# Getting the summary statistics
summary(lm.less.fit)

# Plotting the x and y values
plot(x.less, Y.less)

# Plotting the least square regression line
abline(lm.less.fit, col='red')

# Plotting the population regression line
abline(-1, 0.5, col="blue")

# Creating a legend for plot
legend(-1, c("Predicted", "Actual"), col=1:2, lwd=2)

# When the standard deviations of the error terms are small, that means the 
# predictors will vary less from the response giving the model a better accuracy.

# (i) ####

# Generating data randomly for x from normal distribution
x.more = rnorm(100)

# Generating random error terms with big standard deviation (2) from normal 
# distribution
eps.more = rnorm(100, 0, 2)

# Creating the Y value given x and eps
Y.more = -1 + 0.5*x.more + eps.more

# Fitting a least square regression
lm.more.fit = lm(Y.more~x.more)

# Getting the summary statistics
summary(lm.more.fit)

# Plotting the x and y values
plot(x.more, Y.more)

# Plotting the least square regression line
abline(lm.more.fit, col='red')

# Plotting the population regression line
abline(-1, 0.5, col="blue")

# Creating a legend for plot
legend(-1, c("Predicted", "Actual"), col=1:2, lwd=2)

# When the standard deviations of the error terms are huge, the predictors will 
# vary a lot from the response giving the model a harder time to statistically 
# identify what value it will be.

# (j) ####

# Getting the confidence intervals for each linear model
confint(lm.fit)
confint(lm.less.fit)
confint(lm.more.fit)

# The higher the standard deviation of the error terms, the less confident the model has in 
# predicting a value.

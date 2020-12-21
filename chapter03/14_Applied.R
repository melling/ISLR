# This pproblem focuses on the collinearity problem

# (a) ####

# Setting the random seed
set.seed(0)

# Generating x1 data using runif (provides uniform distribution from 0-1)
x1 = runif(100)

# Generating data for x2 using random values from normal distribution
x2 = 0.5*x1 + rnorm(100)/10

# Creating a linear model. y is a function of x1 and x2
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

## B0 = 2
## B1 = 2
## B2 = 0.3

# (b) ####

# Plotting x1 vs x2
plot(x1, x2)
## Based on the graph, there is a postive correlation between x1 and x2

# Calculating the correlation between x1 and x2
cor(x1, x2) # 0.8324

# (c) ###

# Fitting the a Least Squares Regression to predict y given x1 and x2
lm.fit = lm(y~x1+x2)

# Viewing the statistics of model
summary(lm.fit)

## Bhat0 = 2.0966
## Bhat1 = 2.5370
## Bhat2 = -0.8687

## The coefficient estimatess for Bhat0 and Bhat1 are similar to true coefficients (B0 
## and B1). It is off by at most 0.5 units. 
## However the coefficient estimate for Bhat2 is off by 1.1 units. Its extremely high
## p-value (0.4671) tells us that we cannot reject the null hypothesis, which means 
## that there is a high chance that there is no relationship between y and x2.

# Use VIF to check for multicollinearity 
library(car)
vif(lm.fit)

# (d) ####

# Creating a new model predicting y using only x1
lm.x1 = lm(y~x1)

# Viiewing the statistiics of the model
summary(lm.x1)

## We can reject the null hypothesis because the p-value (4.37E-7) is extremely below 
## 0.05.

# (e) ####

# Creating a new model predicting y using only x2
lm.x2 = lm(y~x2)

# Viiewing the statistiics of the model
summary(lm.x2)

## We can reject the null hypothesis because the p-value (0.0002) is extremely below 
## 0.05.

# (f) ####

## In (c), the p-value for Bhat2 was not siginificant but in (e) if we do a LSR model 
## separately, then it does become significant. Also the coefficient for Bhat2 changed 
## by almost 2 units from (c) and (e).

# (g) ####

# Adding mismeasured observations into the data
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

# Refitting the data with new data
lm.new.fit = lm(y~x1+x2)
summary(lm.new.fit)

# Creating a new model predicting y using only x1
lm.new.x1 = lm(y~x1)

# Viiewing the statistiics of the model
summary(lm.new.x1)

# Creating a new model predicting y using only x1
lm.new.x2 = lm(y~x2)

# Viiewing the statistiics of the model
summary(lm.new.x2)

## Given the new data, it makes the R^2 worse and also the coefficient estimate for 
## Bhat1 is farther that before. 

# Checking for leverage
plot(hatvalues(lm.new.fit))

## Based on the graph above, the new data point has high leverage

# PLotting the model
par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.new.fit)

## Since the studentized residuals for new data is not greater than 3, we can say it 
## is not an outlier.


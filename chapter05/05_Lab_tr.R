#### Lab: Cross-Validation and the Bootstrap ####

#### 5.3.1 The Validation Set Approach ####

# Importing libraries
library(ISLR)

# Setting seed
set.seed(1)

# Getting the indices of train data
train = sample(392, 196)

# Fitting a linear regression on Auto data to predict mpg using only horsepower
lm.fit = lm(mpg ~ horsepower, data=Auto, subset=train)

# Predicting the values in validation set using model
lm.preds = predict(lm.fit, Auto)

# Calculating the MSE of the validation set
mean((Auto$mpg-lm.preds)[-train]^2)

## Comments: The MSE for linear regression is 23.3

# Fitting data using polynomial regression
poly2.fit = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)

# Calculating the MSE of validation set
mean((Auto$mpg-predict(poly2.fit, Auto))[-train]^2) # 18.72

# Fitting data using polynomial regression degree=3
poly3.fit = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)

# Calculating the MSE of validation set
mean((Auto$mpg-predict(poly3.fit, Auto))[-train]^2) # 18.79

# Using a different seed
set.seed(2)
train = sample(392, 196)

# Fitting Simple Linear Regression on new sample
lm.fit = lm(mpg ~ horsepower, data=Auto, subset=train)
mean((Auto$mpg-predict(lm.fit, Auto))[-train]^2) # 25.29

# Fitting data using polynomial regression on new sample
poly2.fit = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)
mean((Auto$mpg-predict(poly2.fit, Auto))[-train]^2) # 20.43

# Fitting data using polynomial regression degree=3 on new sample
poly3.fit = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)
mean((Auto$mpg-predict(poly3.fit, Auto))[-train]^2) # 20.39

## Comments: Changing the trainiing set provides consistent proof that quadric form 
## of horsepower is a better predictor than a linear one to predict mpg.


#### 5.3.2 Leave-One-Out Cross-Validation ####


# imports
library(boot)

# Using glm to fit a linear regression
glm.fit = glm(mpg ~ horsepower, data=Auto)

# Run a K-Fold cross-validation using cv.glm (default k is n, i.e. LOOCV)
cv.err = cv.glm(Auto, glm.fit)

# Viewing the CV estimates using delta
cv.err$delta # 24.2 (both values in delta are the same because LOOCV  is applied
## Comments : delta returns two values: raw CV estimate and adjusted CV to ceompensate 
## for bias,.

# Running a LOOCV for polynomials between 1 and 5
# Creating a placeholder
cv.error = rep(0,5)

# Running a for loop to iterate through each polynomial and fitting data
for (i in 1:5) {
  # Fitting data the polynomial i
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  
  # Saving the CV estimate for the fit
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

# Plotting the results of for loop
plot(c(1:5), cv.error)

## Comments: There is a sharp decline when the polnomial changes from 1 to 2 but then
## plateaus. So it will be better if quadratic form of horsepower is used.

#### 5.3.3 k-Fold Cross-Validation ####

# Setting seed
set.seed(17)

# Creating a placeholder vector with length of 10
cv.error.10 = rep(0, 10)

# Running a for loop to iterate through each polynomial and fitting data
for (i in 1:10) {
  # Fitting a poly regression for each i
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  
  # Applying k-Fold CV to estimate test error
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

# Plotting the results of for loop
plot(1:10, cv.error.10)

## Comments: k-Fold CV was implemented a lot quicker. LOOCV and k-Fold (with k=10) 
## received about the same score.


#### 5.3.4 The Bootstrap ####


# Viewing general info on Portfolio dataset from ISLR
names(Portfolio)
dim(Portfolio)

# Function that returns an estimate on alpha based on index
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}

# Running alpha.fn on Portfolio data
alpha.fn(Portfolio, 1:100) # 0.5758

# Randomly selecting index
set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace=T)) # 0.4630

# Using bootstrap
boot(Portfolio, alpha.fn, R=1000)

# Creating a function that uses bootstrap on Auto
boot.fn = function(data, index) {
  return(coef(lm(mpg ~ horsepower, data=data, subset=index)))
}

# Running boot.fn on Auto
boot.fn(Auto, 1:392)

## Comments: Use bootstrap to assess the variability of the estimates for beta0 
## and beata1. 

# Applying bootstrap to Auto using random subsets
boot.fn(Auto, sample(392, 392, replace=T))

# Using boot function to compute SE of 1000 bootstrap estimates
boot(Auto, boot.fn, 1000)

# Using summary to obtain SE
summary(lm(mpg ~ horsepower, data=Auto))$coef

## Comments: Bootstrap SE will be more accurate than linear regression because linear
## regression makes the assumption that the data is linear which inflates sigma^2


# Applying bootstrap on quadratic form
boot.fn = function(data, index) {
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data=data, subset=index))
}

# Running boostrap
boot(Auto, boot.fn, 1000)

# Running summary to compare to bootstrap
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

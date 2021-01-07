#### 6.7 Lab 3: PCR and PLS Regression ####

# Importing libraries
library(ISLR)
library(pls)

# Setting seed
set.seed(2)

#### 6.7.1 Principal Components Regression ####

# Removing all rows with missing values
Hitters = na.omit(Hitters)

# Fitting a PCR on data
pcr.fit = pcr(Salary ~ ., data=Hitters, scale=TRUE, validation="CV")
'
Comments:
scale=TRUE, standardizes each predictor
validation="CV", computes 10-Fold CV for each possible value of M
'

# Viewing the summary of the fit
summary(pcr.fit)
'
Both CV errors and variance are provided using the summary function
'

# Plotting the MSE 
validationplot(pcr.fit, val.type="MSEP")

# Setting the seed
set.seed(1)

# Organizing features (X) and the target (y) variables
x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary

# Creating a boolean vector for train data
train = sample(1:nrow(x), nrow(x)/2)

# Created a test vector by choosing indicies that are not part of train
test = (-train)

# Getting the response variables for the test data
y.test = y[test]

# Fitting the scaled data using PCR and implementing CV
pcr.fit = pcr(Salary ~ ., data=Hitters, subset=train, scale=TRUE, validation="CV")

# Visualizing the errors
validationplot(pcr.fit, val.type="MSEP") # M = 7 gives the lowest CV error

# Predicting test data using M = 7
pcr.pred = predict(pcr.fit, x[test,], ncomp=7)

# Calculating the MSE
mean((pcr.pred - y.test)^2)

# Applying PCR using M = 7 on entire dataset
pcr.fit = pcr(y ~ x, scale=TRUE, ncomp=7)
summary(pcr.fit)

#### 6.7.2 Partial Least Squares ####

# Fitting PLS on data including the response
set.seed(1)
pls.fit = plsr(Salary ~ ., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

# Predicting on the test data using the fitted PLSR
pls.pred = predict(pls.fit, x[test,], ncomp=2)

# Calculating the MSE
mean((pls.pred - y.test)^2)

# Fitting data with 2 components
pls.fit = plsr(Salary ~ ., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

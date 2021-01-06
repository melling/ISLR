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

# Plotting the MSE 
validationplot(pcr.fit, val.type="MSEP")

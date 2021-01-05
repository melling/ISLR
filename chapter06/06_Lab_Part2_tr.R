#### 6.6 Lab 2: Ridge Regression and the Lasso ####

# Reading in library
library(ISLR)
library(glmnet)

# Removing all rows with missing values
Hitters = na.omit(Hitters)

# Organizing features (X) and the target (y) variables
x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary


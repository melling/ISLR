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
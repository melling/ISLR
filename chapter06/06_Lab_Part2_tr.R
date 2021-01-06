#### 6.6 Lab 2: Ridge Regression and the Lasso ####

# Reading in library
library(ISLR)
library(glmnet)

# Removing all rows with missing values
Hitters = na.omit(Hitters)

# Organizing features (X) and the target (y) variables
x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary

#### 6.6.1 Ridge Regression ####

# Creating a grid of lambda values
grid = 10^seq(10, -2, length=100)

# Applying ridge 
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)

## Comments: glmnet() automatically scales the variables
## To turn off set parameter standaedize=False

dim(coef(ridge.mod))

# Viewing coeficients on the 50th index of lambda values
ridge.mod$lambda[50] # lambda = 11498
coef(ridge.mod)[,50]

# Calculating the l2 norms of the 50th index of lambda values
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Viewing coeficients of the 60th index of lambda values
ridge.mod$lambda[60] # lambda = 705
coef(ridge.mod)[,60]

# Calculating the l2 norms of the 60th index of lambda values
sqrt(sum(coef(ridge.mod)[-1,60]^2))

## Comments: higher l2 norms are associated with lower lambda values

# Using predict to obtain ridge regression coefficients for lambda = 50
predict(ridge.mod, s=50, type="coefficients")[1:20,]

# Setting the seed
set.seed(1)

# Creating a boolean vector for train data
train = sample(1:nrow(x), nrow(x)/2)

# Created a test vector by choosing indicies that are not part of train
test = (-train)

# Getting the response variables for the test data
y.test = y[test]

# Fitting a ridge regression model
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thres=1e-12)

# Using ridge regression to predict on test data using lambda = 4 (arbitrary)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])

# Calculating the MSE
mean((ridge.pred - y.test)^2)

# Using CV to determine the best lambda
## Using built-in-function cv.glmnet() to CV a ridge regression
cv.out = cv.glmnet(x[train,], y[train], alpha=0)

## Plotting the cv results
plot(cv.out)

## Determining the best lambda
bestlam = cv.out$lambda.min
bestlam # 168.651

# Using best lambda from CV to predict on test data
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred - y.test)^2)

# Fitting all the data using the new lambda from CV
out = glmnet(x, y, alpha=0)

# Viewing the coefficients of the best lambda
predict(out, type="coefficients", s=bestlam)[1:20,]

#### 6.6.2 The Lasso ####

# Using lasso to fit data (set alpha=1 for lasso)
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
'Comments: Some of the coefficients will be set to 0 based on the plot'

# Performing cross-validation on lasso regression
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

# Getting the best lambda value
bestlam = cv.out$lambda.min

# Applying lasso with best lambda on test data
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])

# Calculating the MSE
mean((lasso.pred - y.test)^2)

# Viewing the coefficients of the estimate
out = glmnet(x, y, alpha=1, lambda=grid)
lasso.ceof = predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.ceof
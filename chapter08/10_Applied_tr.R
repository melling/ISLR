#### 10 ####

# Imports
library(gbm)
library(ISLR)

#### 10(a) ####

# Removing all rows with missing values
hitters = na.omit(Hitters)

# Log-transforming the salaries
hitters$Salary = log1p(hitters$Salary)

#### 10(b) ####

# Setting the seed
set.seed(1)

# Creating a boolean vector of 200 indicies for train
train = sample(1:nrow(hitters), 200)

#### 10(c) ####

# Creating a list of shrinkage parameters
shrinkage.parameters = c(2, 4, 6, 8, 10)

# Training MSE placeholder vector
train.mse = rep(0, length(shrinkage.parameters))

# Test MSE placeholder vector
test.mse = rep(0, length(shrinkage.parameters))

# Fitting a boosting model trying all values in shrinkage.parameters
for (i in 1:length(shrinkage.parameters)) {
  
  # Fitting a boosting model with 1000 trees and shrinkage.parameters[i]
  boost.hitters = gbm(
    Salary~.,
    data=hitters[train,],
    distribution="gaussian",
    n.trees=1000,
    interaction.depth=shrinkage.parameters[i]
  )
  
  # Predicting on the train data
  yhat.train = predict(boost.hitters, newdata=hitters[train,], n.trees=1000)
  
  # Calculating the train MSE and storing in the train.mse vector
  train.mse[i] = mean((yhat.train-hitters[train,]$Salary)^2)
  
  # Predicting on the test data
  yhat.test = predict(boost.hitters, newdata=hitters[-train,], n.trees=1000)
  
  # Calculating test MSE and storing in test,mse vector
  test.mse[i] = mean((yhat.test-hitters[-train,]$Salary)^2)
}

# Plotting the training MSE for each shrinkage parameter used
plot(shrinkage.parameters, train.mse, type="o", col="blue", pch="o", lty=1)

#### 10(d) ####

' COMMENTS:
Calculating the test MSE in 10(e)
'

# Plotting the test MSE for each shrinkage parameter used
plot(shrinkage.parameters, test.mse, type="o", col="blue", pch="o", lty=1)

#### 10(e) ####

# Fitting a Linear Model

## Fitting a linear model on hitters
lm.fit = lm(Salary~., data=hitters, subset=train)

## Predicting on the test set
lm.preds = predict(lm.fit, newdata=hitters[-train,])

## Calculating the MSE of linear model
lm.mse = mean((lm.preds-hitters[-train,]$Salary)^2)

# Fitting a Lasso Model

## Imports
library(glmnet)

## Preparing the data
x = model.matrix(Salary ~ ., hitters)[,-1]
y = hitters$Salary

## Creating a grid of lambda values
grid = 10^seq(10, -2, length=100)

## Fitting a Lasso model
lasso.fit = glmnet(
  x[train,],
  y[train],
  alpha=1,
  lambda=grid
)

## Plotting the results of lasso
plot(lasso.fit)

## Running a CV to determine the best lambda
cv.lasso = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.lasso)

## Getting the best lambda value
bestlam = cv.lasso$lambda.min

## Applying lasso with best lambda on test data
lasso.pred = predict(lasso.fit, s=bestlam, newx=x[-train,])

## Calculating the MSE
lasso.mse = mean((lasso.pred - y[-train])^2)

## Comparing test MSE for boosting, linear regression, and lasso regression
test.mse[3];lm.mse;lasso.mse

' COMMENTS:
The boosting gave the lowest test MSE, then the lasso, and last the 
linear model
'

#### 10(f) ####

summary(boost.hitters)
' COMMENTS:
The top predictors that have influence on Salary are:
"CRuns", "CRBI", "CAtBat", "CHits", "CWalks", "PutOuts"
'

#### 10(g) ####

# Imports
library(randomForest)

# Fitting a bagging model
bagging.fit = randomForest(
  Salary~.,
  data=hitters,
  subset=train,
  mtry=dim(hitters)[2] - 1,
  importance=TRUE
)

# Predicting test data using bagging model
yhat.bag = predict(bagging.fit, newdata=hitters[-train,])

# Calculating the MSE of bagging model
bag.mse = mean((yhat.bag-hitters[-train,]$Salary)^2)

# Comparing boosting test mse with bagging
test.mse[3]; bag.mse

' COMMENTS:
Bagging has a lower MSE than boosting
'
#### 6.5.1 Best Subset Selection ####

# Reading in library
library(ISLR)

# Viewing basic info on Hitters dataset
names(Hitters)
dim(Hitters)

# Viewing the number of missing values in the Salary column
sum(is.na(Hitters$Salary))

# Removing all rows with missing values
Hitters = na.omit(Hitters)

# Viewing the dim after removing missing values in Salaries
dim(Hitters)

# Performing best subset selection

## Reading in library for regsubsets function
library(leaps)

## Fitting regression using regsubsets
regfit.full = regsubsets(Salary ~ ., data=Hitters)

# Viewing the summary
summary(regfit.full)

## Comments: An asterick indicates that the variable is included in the correspondig model.
## By default, regsubsets() returns the best 8 models. Use nvmax() to get more models

## Running a best subset selection and return more models with nvmax()
regfit.full = regsubsets(Salary ~ ., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)

## Getting the R^2, RSS, adjusted R^2, Cp, and BIC from summary
names(reg.summary)

## Plotting the estimated test errors 

### Setting the plot formation
par(mfrow=c(2,2))

### Plotting the relationship between the number of variables and RSS
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")

### Plotting the relationship between the number of variables and RSS
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="adjusted R^2", type="l")

### Plotting a red dot indicating model with the largest adjusted R^2
points(which.max(reg.summary$adjr2), reg.summary$adjr2[11], col="red", cex=2, pch=20)

### Plotting Cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(which.min(reg.summary$cp), reg.summary$cp[10], col="red", cex=2, pch=20)

## Plotting BIC
plot(reg.summary$cp, xlab="Number of Variables", ylab="BIC", type="l")
min.bic = which.min(reg.summary$bic)
points(min.bic, reg.summary$bic[min.bic], col="red", cex=2, pch=20)

## Plotting with the built in function
par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

## Comments: Black square represents a varaible selected in the optimal model

# Viewing the coefficient estimates
coef(regfit.full, 6)

#### 6.5.2 Forward and Backward Stepwise Selection ####

# Fitting a forward model (set method parameter to "forward")
regfit.fwd = regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)

# Fitting a backward model (set method parameter to "backward")
regfit.bwd = regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

# Comparing coefficient estimates for each stepwise selection
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#### 6.5.3 Choosing Among Models Using the Validation Approachs ####


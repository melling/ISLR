#### 08 Applied ####

# Importing Libraries

library(leaps)

#### 8(a) ####

# Setting the seed
set.seed(1)

# Generating data for X
X = rnorm(100)

# Generating error vector
e = rnorm(100, 0, .5)

#### 8(b) ####

# Setting the beta values
beta0 = 4
beta1 = 3
beta2 = 2
beta3 = 1

# Creating data for Y using the formula
Y = beta0 + beta1*X + beta2*X^2 + beta3*X^3 + e

# Plotting function
plot(X, Y)

#### 8(c) ####

# Creating a dataframe with X and Y
df = data.frame(X,Y)

# Fitting best subset selection fit using a poly of 10
bss.fit = regsubsets(Y ~ poly(X, 10), data=df)

# Viewing the best j-variable fits
bss.fit.summary = summary(bss.fit)
bss.fit.summary

# Plotting the errors
par(mfrow=c(1,1))

## Plotting the relationship between the number of variables and RSS
plot(bss.fit.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
points(which.min(bss.fit.summary$rss), 
       bss.fit.summary$rss[which.min(bss.fit.summary$rss)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting the relationship between the number of variables and adjusted R^2
plot(bss.fit.summary$adjr2, xlab="Number of Variables", ylab="adjusted R^2", type="l")

## Plotting the relationship between the number of variables and Cp
plot(bss.fit.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(which.min(bss.fit.summary$cp), 
       bss.fit.summary$cp[which.min(bss.fit.summary$cp)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting the relationship between the number of variables and BIC 
plot(bss.fit.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(bss.fit.summary$bic), 
       bss.fit.summary$bic[which.min(bss.fit.summary$bic)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting with the built in function
par(mfrow=c(1,1))
plot(bss.fit, scale="r2")
plot(bss.fit, scale="adjr2")
plot(bss.fit, scale="Cp")
plot(bss.fit, scale="bic")

'Based on the graphs above: 
- r2 says the 8-variable model is the best model. However r^2 is not consider the number 
of variables
- adjusted r^2 says a 5-variable model is enough to represent the data
- Cp says a 4-variable model is enough
- BIC says the best model is a 3-variable model'

# Viewing the coefficient estimates based on the 3-variable model
coef(bss.fit, 3)

#### 8(d) ####

# Fitting the data using a forward stepwise selection
fss.fit = regsubsets(Y ~ poly(X, 10, raw=T), data=df, nvmax=10, method="forward")

# Viewing the best j-variable fits
fss.fit.summary = summary(fss.fit)
fss.fit.summary

# Plotting the errors
par(mfrow=c(2,2))

## Plotting the relationship between the number of variables and RSS
plot(fss.fit.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
points(which.min(fss.fit.summary$rss), 
       fss.fit.summary$rss[which.min(fss.fit.summary$rss)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting the relationship between the number of variables and adjusted R^2
plot(fss.fit.summary$adjr2, xlab="Number of Variables", ylab="adjusted R^2", type="l")
points(which.max(fss.fit.summary$adjr2), 
       fss.fit.summary$adjr2[which.max(fss.fit.summary$adjr2)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting the relationship between the number of variables and Cp
plot(fss.fit.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(which.min(fss.fit.summary$cp), 
       fss.fit.summary$cp[which.min(fss.fit.summary$cp)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting the relationship between the number of variables and BIC 
plot(fss.fit.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(fss.fit.summary$bic), 
       fss.fit.summary$bic[which.min(fss.fit.summary$bic)], 
       col="red", 
       cex=2, 
       pch=20)

## Plotting with the built in function
par(mfrow=c(1,1))
plot(fss.fit, scale="r2")
plot(fss.fit, scale="adjr2")
plot(fss.fit, scale="Cp")
plot(fss.fit, scale="bic")

# Viewing the coefficient estimates based on the 3-variable model
coefficients(fss.fit, 3)

'Based on the graphs of the forward stepwise selection:
- RSS says that the best model is a 5-variable model but doesnt include all the variables
- adjusted R^2 says that the best model is a 5-variable model
- Cp says that the best model is a 4-variable model
- BIC says that the best model is a 3-variable model'

# Fitting data sing backward stepwise selection
backss.fit = regsubsets(Y ~ poly(X, 10, raw=T), data=df, nvmax=10, method="backward")

# Viewing the best j-variable fits
backss.fit.summary = summary(backss.fit)
backss.fit.summary

## Plotting with the built in function
par(mfrow=c(1,1))
plot(backss.fit, scale="r2")
plot(backss.fit, scale="adjr2")
plot(backss.fit, scale="Cp")
plot(backss.fit, scale="bic")

# Viewing the coefficient estimates based on the 3-variable model
coefficients(backss.fit, 3)

'Based on the graphs of the forward stepwise selection:
- adjusted R^2 says that the best model is a 5-variable model
- Cp says that the best model is a 4-variable model
- BIC says that the best model is a 3-variable model'

#### 8(e) ####

# Importing library for lasso
library(glmnet)

# Setting the seed
set.seed(1)

# Fitting a lasso regression on df
lasso.fit = cv.glmnet(poly(df$X, 10), df$Y, alpha=1)

# PLotting the lambdas vs MSE
plot(lasso.fit)

# Getting the best lambda, the minimm
best.lambda = lasso.fit$lambda.min

# Refiting the data
best.model = glmnet(poly(df$X, 10), df$Y, alpha=1)

# Predicting using the best lambda
predict(best.model, s=best.lambda, type="coefficients")

'Based on the model, X, X^2, X^3 are very significant and X^4, X^5, and X^10 are also included with 
little significance.'
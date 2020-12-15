setwd("../chapter03/")
#getwd()

library(MASS)
library(ISLR)
# install.packages("ISLR")
# data(package = "ISLR") # View included datasets

## Simple Linear Regression ####

names(Boston)
?Boston

# mdev = median value of owner-occupied homes in \$1000s
# lstate = lower status of the population (percent).

lm.fit = lm(medv~lstat, data = Boston)

attach(Boston)
lm.fit = lm(medv~lstat) # mdev is the response, lstat is the predictor

lm.fit

summary(lm.fit)

names(lm.fit)
coef(lm.fit)

# one line for each lstat, c(5,10,15)
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence") # Confidence Intervals

predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction") # Prediction Intervals

plot(lstat,medv)
abline(lm.fit) # Add Least Squares Regression Line

abline(lm.fit, lwd=3) # Line width
abline(lm.fit, lwd=3, col="red") # color
abline(lm.fit, col="red")
abline(lm.fit, pch=20) # Plot character
abline(lm.fit, pch="+") # Plot character
abline(lm.fit, pch=1:20) # Plot character

par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit) # 4 plots
par()
par(mfrow = c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # Largest leverage statistic

## Multiple Linear Regression ####

lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit = lm(medv ~ . , data = Boston) # Use all 13 variables
summary(lm.fit)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# install.packages("car")
library(car)
vif(lm.fit) # Variance inflation factors

lm.fit1 = lm(medv ~ . -age, data = Boston) # Use all 13 variables - age
summary(lm.fit1)

update(lm.fit, ~. -age) # Another way to remove age
summary(lm.fit)


## Interaction Terms ####

summary(lm(medv ~ lstat * age, data = Boston))

## Non-Linear Transformations of the Predictors

lm.fit2 = lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit2) # 4 plots

### Polynomial Fit ####

lm.fit5 = lm(medv ~ poly(lstat,5)) # 5th degree fit
summary(lm.fit5)

lm.fit_log = lm(medv ~ log(rm), data = Boston) # log transformation rooms
summary(lm.fit_log)

### Qualitative Predictors ####

names(Carseats)

lm.fit <- lm(Sales ~ .+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)

contrasts(ShelveLoc) # Return coding for dummy variables

?contrasts

LoadLibrary=function() {
  library(MASS)
  library(ISLR)
  print("Libraries loaded")
}

LoadLibrary()

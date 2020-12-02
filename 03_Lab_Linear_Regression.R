library(MASS)
library(ISLR)
# install.packages("ISLR")
# data(package = "ISLR") # View included datasets

## Simple Linear Regression ####

names(Boston)
?Boston

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


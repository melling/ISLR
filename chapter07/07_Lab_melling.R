# Chapter 7 Lab

## Splines ####

library(ISLR)
library(MASS)
library(splines)



#### Lab: Non-linear Modeling ####

library(ISLR)

#### 7.8.1 Polynomial Regression and Step Functions ####

# Fitting a linear model using a 4th degree polynomial
fit = lm(wage ~ poly(age,4), data=Wage)
summary(fit)
'COMMENT: raw = false
This means that each column of poly() is a linear combination of variables'

# Viewing the coefficients of the 4th degree polynomial
coef(summary(fit))

# Using poly(raw=TRUE)
fit2 = lm(wage ~ poly(age, 4, raw=TRUE), data=Wage)
coef(summary(fit2))

# Using I() to fit polynomial
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
coef(fit2a)

# Using cbind() to fit polynomial
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)
coef(fit2b)

## Getting the range of ages in Wage
agelims = range(Wage$age)

## Creating a sequence of values in between agelims
age.grid = seq(from=agelims[1], to=agelims[2])

## Predicting the wages for the ages in age.grid using fit model
preds = predict(fit, newdata=list(age=age.grid), se=TRUE)

# Getting confidence interval for each prediction
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

# Plotting the data
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim=agelims, cex=.5, col='darkgrey')
title("Degree-4 Polynomial", outer=TRUE)

## Plotting the best fit line
lines(age.grid, preds$fit, lwd=2, col="blue")

## Plotting the confidence bands
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# Predicting wages using poly(raw=TRUE)
preds2 = predict(fit2, newdata=list(age=age.grid), se=TRUE)

# Checking to see if there is a difference in using poly(raw=TRUE)
max(abs(preds$fit-preds2$fit)) # 7.81597e-11 (no difference)

# Testing to see which degree of polynomial is sufficient using ANOVA

## Fitting 5 different poly fits
fit.1 = lm (wage ~ age, data=Wage)
fit.2 = lm (wage ~ poly(age, 2), data=Wage)
fit.3 = lm (wage ~ poly(age, 3), data=Wage)
fit.4 = lm (wage ~ poly(age, 4), data=Wage)
fit.5 = lm (wage ~ poly(age, 5), data=Wage)

# Running ANOVA (pg.116)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

'
COMMENTS: 
P-values in ANOVA that are around 0.05 is desired. ???? 
  - Too low p-values indeicate that it is not sufficient 
  - Too high p-values indeicate that it is not necessary/justified
Can use ANOVA for both poly(raw=FALSE) or poly(raw=TRUE)
'

# Viewing the p-values using coefs if poly(raw=FALSE) is used
coef(summary(fit.5))

# Fitting a model to predict if person receives over 250k or below
fit = glm(I(wage>250) ~ poly(age, 4), data=Wage, family=binomial)

# Predicting using Logistic Regression
preds = predict(fit, newdata=list(age=age.grid), se=TRUE)

# Inversing the logits to get probabilities
pfit = exp(preds$fit)/(1+exp(preds$fit))

# Getting the confidence bands of logits
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

# Getting the confidence bands of probabilities
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# Predicting using type='response'
preds = predict(fit, newdata=list(age=age.grid), type='response', se=TRUE)

# Plotting
plot(Wage$age, I(Wage$wage>250), xlim=agelims, type="n", ylim=c(0,.2))
points(jitter(Wage$age), I((Wage$wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# Using cut function to apply step function

## Cutting age into 4 bins
table(cut(Wage$age,4))

## Fitting a linear model on each bin
fit = lm(wage ~ cut(age,4), data=Wage)
coef(summary(fit))

## +++++++++++++++++++++++++++++++++++++++
# Splines ####
# p293

# Basic functions
# B-spline or basis spline
#
fit = lm(wage ~ bs(age, knots=c(25,40,60)), data = Wage)
pred = predict(fit, newdata=list(age=age.grid), se=T)

plot(Wage$age,Wage$wage, col="gray")

lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2 * pred$se , lty="dashed")
lines(age.grid, pred$fit - 2 * pred$se , lty="dashed")

dim(bs(Wage$age, knots = c(25,40,60)))
#bs(Wage$age, knots = c(25,40,60))
dim(bs(Wage$age, df=6)) # 7 Degrees of freedom, 6 basis functions
attr(bs(Wage$age, df=6), "knots")

# Natural Spline ####
fit2=lm(wage~ns(age,df=4), data = Wage)
pred2=predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

plot(Wage$age, Wage$wage, xlim = agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
fit= smooth.spline(Wage$age, Wage$wage, df=16)
fit2=smooth.spline(Wage$age, Wage$wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd=2)
lines(fit2, col = "blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"),
       col=c("red", "blue"), lty=1, lwd = 2, cex=.8)

# Local Regression ####
# Uses loess
attach(Wage)
plot(age, wage, xlim = agelims, cex=.5, col="darkgrey")
title("Local Regression")

fit=loess(wage~age, span=.2,data=Wage)
fit2=loess(wage~age, span=.5,data=Wage)

lines(age.grid, predict(fit,data.frame(age=age.grid)),
      col="red", lwd=2)
lines(age.grid, predict(fit2,data.frame(age=age.grid)),
      col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"),
       col=c("red", "blue"), lty=1, lwd = 2, cex=.8)

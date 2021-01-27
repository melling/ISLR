#### Applied 8 ####

library(ISLR)
library(gam)
View(Auto)

pairs(Auto)
# Three predictors we are working on is horsepower, weight, acceleration
poly.hp.fit = lm(mpg ~ poly(horsepower, 4) + weight + acceleration, data=Auto)
summary(poly.hp.fit)

# Applying polynomial regression on horsepower
lm.hp.1 = lm(mpg ~ horsepower + weight + acceleration, data=Auto)
lm.hp.2 = lm(mpg ~ poly(horsepower, 2) + weight + acceleration, data=Auto)
lm.hp.3 = lm(mpg ~ poly(horsepower, 3) + weight + acceleration, data=Auto)
lm.hp.4 = lm(mpg ~ poly(horsepower, 4) + weight + acceleration, data=Auto)

# Applying ANOVA to determine which model is best
anova(lm.hp.1, lm.hp.2, lm.hp.3, lm.hp.4)

## COMMENTS:
# Based on the ANOVA results, the best model is horsepower with either power of 2 or 3

# Applying splines 
library(gam)
library(splines)

s.hp.4 = gam(mpg ~ s(horsepower,4) + acceleration + weight, data=Auto)
s.hp.3 = gam(mpg ~ s(horsepower,3) + acceleration + weight, data=Auto)
s.hp.2 = gam(mpg ~ s(horsepower,2) + acceleration + weight, data=Auto)

# Using anova
anova(s.hp.2, s.hp.3, s.hp.4)
par(mfrow=c(1,3))

plot(s.hp.4, se=TRUE, col='blue')

# Chapter 7 Question 7
# p299

library(tidyverse)
library(ISLR)

attach(Wage)
# View(Wage)
par(mfrow=c(1,1))
pairs(Wage)


## Wage vs maritl ####

plot(maritl,wage) # Old way

ggplot(Wage, aes(x = maritl, y = wage, fill = maritl)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

## Wage vs jobclass ####

plot(jobclass,wage) # Old way

ggplot(Wage, aes(x = jobclass, y = wage, fill = jobclass)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

# Polynomial Regression

# Step Functions
# Use cut()

jobclass.fit = lm(wage ~ jobclass + cut(age,4), data = Wage)
par(mfrow=c(2,2))
plot(jobclass.fit)
summary(jobclass.fit)
#anova(fit1,fit2,fit3)
#cut(Wage$jobclass, 4)

## Splines

# Can't do splines with categorical variables

## GAM - Generalize Additive Models

library(splines)
library(gam)
#attach(Wage)
#fit = gam(wage ~ age + maritl + jobclass , data = Wage)
library(ISLR)
#gam::gam.fit()
fit.gam = gam(wage ~ maritl + jobclass + s(age,5), data = Wage)
deviance(fit.gam)

#gam.m3=gam(wage∼s(year ,4)+s(age ,5)+education ,data=Wage)
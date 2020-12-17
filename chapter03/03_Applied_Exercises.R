# Chapter 3: Applied Exercises

library(ISLR)

# Question 8 ####

# 8a ####
lm.fit = lm(mpg~horsepower, data = Auto)

lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# 8a) i. ####
# Relationship between predictor and response?

#              Estimate Std. Error t value Pr(>|t|)    
# horsepower  -0.157845   0.006446  -24.49   <2e-16 ***

# Answer: Small negative correlation, small p-value

# 8a) ii. ####

# Answer:
# R-squared:  0.6059 is high implies strong correlation



# 8a) iii. ####

# Answer: Negative, because coefficient is negative

# 8a) iv. ####

predict(lm.fit, data.frame(horsepower = c(98)), interval = "prediction") 

# fit     lwr      upr
# 1 24.46708 14.8094 34.12476

# Answer: 24.4 mpg

# calls predict.lm level=0.95
predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence") # 95%


# fit      lwr      upr
# 1 24.46708 23.97308 24.96108

# Answer: 23.97308 24.96108

# 8b ####

plot(horsepower,mpg) # x,y
abline(lm.fit, col=2) # Add Least Squares Regression Line

## Will have high RSS because of fit
## R^2 says we're missing 40% of variance
## Va

# 8c - Diagnostic plots ####
par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit) # 4 plots

# Question 9 ####

# 9a ####
par(mfrow = c(1,1))
pairs(Auto) # All scatterplots

# 9b ####
#
#View(Auto)
cor(Auto[,-9]) # Skip name because it's qualitative
#cor(Auto[,-c("name")]) # Skip name because it's qualitative
#

# 9c ####

lm.fit <- lm(mpg ~ . -name, data = Auto) # Skip name because it's qualitative
summary(lm.fit)

# 9c i.

# p-value p(>|t|) - Choose < 0.05
### *** means very significant 0.001
###  ** means  significant: 0.01

# 9c ii.

## Anything with ** or ***, except for Intercept, of course

# 9c iii.

# 0.750773 means it increases by almost 1 (.75) mpg every year?
# Every 4 years, we get 3 more miles per gallon

# 9d  lm Diagnostic plots ####

par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit) # Diagnostic plots

# Section 3.3.3 p93 Figure 3.9 Residuals vs Fitted

## Residual plot suggests ? about Outliers?  Fig 3.12 p97

### Observation 14 the Station Wagon has high leverage??


# 9e Interaction Effects ####

## weight * year means weight, year, and weight:year (the interaction term)
summary(lm(mpg ~ weight * year, data = Auto))

## : means only interaction, no main effects (predictors that created the term e.g no displacement, weight)
summary(lm(mpg ~ displacement:weight, data = Auto))

# 9f Variable Transformation ####

lm.fit = lm(mpg ~ weight + I(weight^2))
summary(lm.fit)

lm.fit = lm(mpg ~ weight + log(weight))
summary(lm.fit)

lm.fit = lm(mpg ~ weight + sqrt(weight))
summary(lm.fit)

# 10a ####

lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)
par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit)

# 10b ####

# Price - Large t-value
# UrbanYes - qualitative
# USYes - qualitative

# 10c ####

# Sales = Beta0 + Beta1 * Price - Beta2 * Urban + Beta3 * US 
# Use dummy variables for qualitative

# Beta2 = -1 if Urban, 0 otherwise - Negative?
# Beta3 = 1 if US, 0 otherwise - Positive
# Price Coefficient = -0.054459 negative effect


# 10d ####

# Which predictors reject null hypothesis?  Beta_j=0

# Low p-values predictors: Price, USYes


# 10e: Better Formula ####

# ??
# Sales = Beta0 + Beta1 * Price + Beta2 * US 

carseat.fit2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseat.fit2)
par(mfrow = c(2,2)) # 4 plots in same picture
plot(carseat.fit2)

# 10f: Better Fit? ####

# The two plot(lm.fit) Look almost the same

##
# 10g: 95% CI ####

confint(carseat.fit2)


# 10h: Evidence for outliers ####

# Section 3.3.3 Potential Problems: #4
# Can tell outliers because they show up ...

# Average leverage = (p+1)/n 
# (3+1)/400 = 0.0076
# Values greater than (p+1)/n are high leverage
dim(Carseats)
# (Predictors, Price + 1) / number of rows
(2+1)/dim(Carseats[1])

(2+1)/400
# Visualizing the statistics for leverage
par(mfrow = c(2,2))
plot(carseat.fit2)

# +++++++++++++++++++++++++++++++++++++++++++

# 11 ####

set.seed(1)
x = rnorm(100)
y = 2* x * rnorm(100)

# 11a ####

lm(y~x) # Wrong!!!

# Intercept = -0.37901     x= -0.08298  

# Tells us to use x+0 in question
# More References:
# - https://stackoverflow.com/questions/7333203/linear-regression-with-a-known-fixed-intercept-in-r
# - https://rpubs.com/aaronsc32/regression-through-the-origin

lm.fit = lm(y ~ x + 0) # Need to add x intercept
summary(lm.fit)

# x coefficient = -0.4508
# p-value: 0.00508

# p-value near 0, reject null hypothesis that B_1 = 0


# 11b ####

lm.fit = lm(x ~ y + 0) # Need to add x intercept
summary(lm.fit)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# y  -0.1699     0.0593  -2.866  0.00508 **

# p-value near 0, reject null hypothesis that B_1 = 0

# 11c ####


# 11d: Skipped ####

# 11e: Skipped ####

# 11f ####


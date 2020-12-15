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

summary(lm(mpg ~ weight * year, data = Auto))

summary(lm(mpg ~ displacement:weight, data = Auto))

# 9f Variable Transformation ####

lm.fit = lm(mpg ~ weight + I(weight^2))
summary(lm.fit)

lm.fit = lm(mpg ~ weight + log(weight))
summary(lm.fit)

lm.fit = lm(mpg ~ weight + sqrt(weight))
summary(lm.fit)

# Misc ####

lm.fit = lm(mpg ~ . , data = Auto) # Use all  variables
summary(lm.fit)

par(mfrow = c(2,2)) # 4 plots in same picture


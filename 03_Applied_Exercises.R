# Chapter 3: Applied Exercises

library(ISLR)

# 8a
lm.fit = lm(mpg~horsepower, data = Auto)

lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# 8a) i. 
# Relationship between predictor and response?

#              Estimate Std. Error t value Pr(>|t|)    
# horsepower  -0.157845   0.006446  -24.49   <2e-16 ***

# Answer: Small negative correlation, small p-value

# 8a) ii.

# Answer:
# R-squared:  0.6059 is high implies strong correlation



# 8a) iii.

# Answer: Negative, because coefficient is negative

# 8a) iv.

predict(lm.fit, data.frame(horsepower = c(98)), interval = "prediction") 

# fit     lwr      upr
# 1 24.46708 14.8094 34.12476

# Answer: 24.4 mpg

# calls predict.lm level=0.95
predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence") # 95%


# fit      lwr      upr
# 1 24.46708 23.97308 24.96108

# Answer: 23.97308 24.96108

# 8b)

plot(horsepower,mpg) # x,y
abline(lm.fit, col=2) # Add Least Squares Regression Line

## Will have high RSS because of fit
## R^2 says we're missing 40% of variance
## Va

# 8c
par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.fit) # 4 plots

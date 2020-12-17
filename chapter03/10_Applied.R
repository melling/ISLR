library(ISLR)
library(MASS)

# (a)
names(Carseats)
carseat.fit = lm(Sales ~ Price + Urban + US, data=Carseats)

# (b)
summary(carseat.fit)
?Carseats
contrasts(Carseats$US)
# USYes coefficient: If the store is in the US (predictor=1), the sales increase 
# at a rate ofthe coefficient is 1.2. 
# sales = 1.2 * USYes (1.2 from coefficient from model) (ignoring other predictors for simplicity)

# Price: Price is highly significant (p-value) when it comes to sales. There is a slight 
# negative correlation to sales. As prices goes up, sales go down.

# UrbanYes: Does not have a significant p-value. This means that it does not effect the sales.
# Consider removing from model

# (c)
# Sales = 13.04 - 0.05 * Price + 1.2 * US
# 1 if US = Yes ; 0 if US = No

# (d)
# We can reject the null hypothesis for Price and USYes because its p-value
# is highly significant (<<.05)

# (e)
carseat.fit2 = lm(Sales ~ Price + US, data=Carseats)
summary(carseat.fit2)
plot(carseat.fit2)

# (f)
# The models for both (a) and (e) do NOT fit the data well. The R^2 statistic for both 
# models show that the model ONLY explains 23% of the variance.  

# (g)
# Getting the confidence intervals for each coefficient
confint(carseat.fit2)

# (h)
# Visualizing the statistics for leverage
par(mfrow = c(2,2))
plot(carseat.fit2)

# Based on the Residuals vs. Leverage graph (bottom right):
# There is one observation that is far right of the graph. This means that its leverage is 
# really high. Also there are few more that are few more that have high leverage.



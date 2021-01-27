#### 7.8.3 GAMs ####

library(ISLR)
library(splines)
library(gam)

#### Fitting data using lm() ####
## Using ns as the basis function for predictors, year and age
gam1.model = lm(wage ~ ns(year,4) + ns(age,5) + education, data=Wage)

# Fitting GAM using Splines ####
gam.m3 = gam(wage ~ s(year,4) + s(age,5) + education, data=Wage)

# Plotting the summary
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
## COMMENTS:
# plot() recognizes gam class and invokes plot.gam()

# Plotting gam1 using plot.gam()
plot.Gam(gam1.model, se=TRUE, col='red')
## COMMENTS:
# The predictor, year, looks linear
# Perform ANOVA tests on three models to determine how to deal with 'year'

# First Model: apply a GAM excluding 'year'
gam.m1 = gam(wage ~ s(age,5) + education, data=Wage)

# Second Model: apply GAM that uses a linear function on 'year'
gam.m2 = gam(wage ~ year + s(age,5) + education, data=Wage)

# Third Model: apply GAM that uses a spline function on 'year'
gam.m3 = gam(wage ~ s(year,4) + s(age,5) + education, data=Wage)

#### Running ANOVA on gam.m1, gam.m2, gam.m3 ####
anova(gam.m1, gam.m2, gam.m3, test="F")

## COMMENTS:
# Based on the ANOVA results, model with linear function on 'year' is the best. 
# There is no evidence (p-value -> 0.3485) that a non-linear function of 'year'
# is needed.

# Viewing the summary of gam.m3
summary(gam.m3)

## COMMENTS:
# The spline function on 'year' has a high p-value which means it doesn't have a 
# correlation with wage. The spline function on age has a low p-value which 
# indicates that a non-linear function on age has significance on wage

# Predicting using GAM model
preds = predict(gam.m2, newdata=Wage)

#### Adding local regression to a GAM fit ####
gam.lo = gam(wage ~ s(year,df=4) + lo(age,span=0.7) + education, data=Wage) 

# Plotting the GAM fit
plot.Gam(gam.lo, se=TRUE, col="green")

#### Using interaction terms with local regression ####
gam.lo.i = gam(wage ~ lo(year, age, span=0.5) + education, data=Wage)

# Plotting gam.lo.i using akima
library(akima)
plot(gam.lo.i)

#### Fitting a GAM using a Logistic Regression ####
gam.lr = gam(I(wage>250) ~ year + s(age,df=5) + education, family=binomial, data=Wage)

# Plotting GAM results
par(mfrow=c(1,3))
plot(gam.lr, se=TRUE, col="green")

# 
table(Wage$education, I(Wage$wage>250))

fit.gam = gam(wage ~ maritl + jobclass + s(age,5), data=Wage)
summary(fit.gam)

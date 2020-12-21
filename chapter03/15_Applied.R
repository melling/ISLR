# Reading in libraries
library(MASS)

# Attaching the Boston dataset to workspace
attach(Boston)

lm.function = function(predictor) {
  
  fit1 <- lm(crim ~ predictor, data = Boston)
  #fit1$coefficients
  # names(fit1$coefficients) <- c('Intercept', predictor)
  return(summary(fit1))
}

# for (v in c(rm, age)) {
#   summary(lm(crim ~ v, data = Boston))
# }
# lm.function(rm)

# 15a ####

# Creating a SLR model to predict crim using zn
lm.zn = lm(crim ~ zn, data = Boston)
summary(lm.zn)
# Based on the p-value (5.51e-6), zn has a significant association with crim

# Plotting diagnostic plots
par(mfrow = c(2,2))
plot(lm.zn)

# Creating a SLR model to predict crim using indus
lm.indus = lm(crim ~ indus, data = Boston)
summary(lm.indus)
# Based on the p-value (2e-16), indus has a significant association with crim

# Creating a SLR model to predict crim using chas
lm.chas = lm(crim ~ chas, data = Boston)
summary(lm.chas)
# Based on the p-value (.209), chas does not have an association with crim

# Creating a SLR model to predict crim using nox
lm.nox = lm(crim ~ nox, data = Boston)
summary(lm.nox)
# Based on the p-value (2e-16), nox has a significant association with crim

# Creating a SLR model to predict crim using rm
lm.rm = lm(crim ~ rm, data = Boston)
summary(lm.rm)
# Based on the p-value (6.35e-7), rn has a significant association with crim

# Creating a SLR model to predict crim using age
lm.age = lm(crim ~ age, data = Boston)
summary(lm.age)
# Based on the p-value (2.85e-16), age has a significant association with crim

# Creating a SLR model to predict crim using dis
lm.dis = lm(crim ~ dis, data = Boston)
summary(lm.dis)
# Based on the p-value (2e-16), dis has a significant association with crim

# Creating a SLR model to predict crim using rad
lm.rad = lm(crim ~ rad, data = Boston)
summary(lm.rad)
# Based on the p-value (2e-16), rad has a significant association with crim

# Creating a SLR model to predict crim using tax
lm.tax = lm(crim ~ tax, data = Boston)
summary(lm.tax)
# Based on the p-value (2e-16), tax has a significant association with crim

# Creating a SLR model to predict crim using ptratio
lm.ptratio = lm(crim ~ ptratio, data = Boston)
summary(lm.ptratio)
# Based on the p-value (2.94e-11), ptratio has a significant association with crim

lm.black = lm(crim~black)

# Creating a SLR model to predict crim using lstat
lm.lstat = lm(crim ~ lstat, data = Boston)
summary(lm.lstat)
# Based on the p-value (2e-16), lstat has a significant association with crim

# Creating a SLR model to predict crim using medv
lm.medv = lm(crim ~ medv, data = Boston)
summary(lm.medv)
# Based on the p-value (2e-16), tax has a significant association with crim

plot(lm.indus)
plot(lm.chas)
plot(lm.nox)
plot(lm.rm)
plot(lm.age)
plot(lm.dis)
plot(lm.rad)
plot(lm.tax)
plot(lm.ptratio)
plot(lm.lstat)
plot(lm.medv)

# 15b ####

# Fitting a multiple linear regression
lm.all = lm(crim ~ ., data=Boston)

# Viewing the statistics for the MLR model
summary(lm.all)

# Based on the MLR, only zn, dis, rad, black, and medv have a significant association
# with crim (p-value is below 0.05) which means we can reject the nuil hypothesis

# 15c ####

### FIXME: for loop melling
## FIXME: melling
### FIXME: 15d T

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]

par(mfrow = c(1,1)) # 1 plot
plot(x, y)

# x$coeffients <-  c('Intercept', "RME")


# 15d ####
# Creating a SLR model to predict crim using zn
lm.poly.zn = lm(crim ~ zn + I(zn^2) + I(zn^3), data = Boston)
summary(lm.poly.zn)
# Based on the p-values, zn does NOT have a non-linear association with crim

# Plotting diagnostic plots
par(mfrow = c(2,2))
plot(lm.zn)

# Creating a SLR model to predict crim using indus
lm.poly.indus = lm(crim ~ indus + I(indus^2) + I(indus^3), data = Boston)
summary(lm.poly.indus)
# Based on the p-values, indus SHOWS that it has a non-linear association with crim

# Creating a SLR model to predict crim using chas
lm.poly.chas = lm(crim ~ chas + I(chas^2) + I(chas^3), data = Boston)
summary(lm.poly.chas)
# Since chas is a factor, squaring it does not affect it.

# Creating a SLR model to predict crim using nox
lm.poly.nox = lm(crim ~ nox + I(nox^2) + I(nox^3), data = Boston)
summary(lm.poly.nox)
# Based on the p-values, nox SHOWS that it has a non-linear association with crim

# Creating a SLR model to predict crim using rm
lm.poly.rm = lm(crim ~ rm + I(rm^2) + I(rm^3), data = Boston)
summary(lm.poly.rm)
# Based on the p-value, rm does NOT have a non-linear association with crim

# Creating a SLR model to predict crim using age
lm.poly.age = lm(crim ~ age + I(age^2) + I(age^3), data = Boston)
summary(lm.poly.age)
# Based on the p-values, age SHOWS a non-linear association with crim

# Creating a SLR model to predict crim using dis
lm.poly.dis = lm(crim ~ dis + I(dis^2) + I(dis^3), data = Boston)
summary(lm.poly.dis)
# Based on the p-values, dis SHOWS a non-linear association with crim

# Creating a SLR model to predict crim using rad
lm.poly.rad = lm(crim ~ rad + I(rad^2) + I(rad^3), data = Boston)
summary(lm.poly.rad)
# Based on the p-value, rad does NOT have a non-linear association with crim

# Creating a SLR model to predict crim using tax
lm.poly.tax = lm(crim ~ tax + I(tax^2) + I(tax^3), data = Boston)
summary(lm.poly.tax)
# Based on the p-value, tax does NOT have a non-linear association with crim

# Creating a SLR model to predict crim using ptratio
lm.poly.ptratio = lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3), data = Boston)
summary(lm.poly.ptratio)
# Based on the p-value, ptratio SHOWS a non-linear association with crim

# Creating a SLR model to predict crim using lstat
lm.poly.lstat = lm(crim ~ lstat + I(lstat^2) + I(lstat^3), data = Boston)
summary(lm.poly.lstat)
# Based on the p-value, lstat NOT have a non-linear association with crim

# Creating a SLR model to predict crim using medv
lm.poly.medv = lm(crim ~ medv + I(medv^2) + I(medv^3), data = Boston)
summary(lm.poly.medv)
# Based on the p-value, medv SHOWS a non-linear association with crim

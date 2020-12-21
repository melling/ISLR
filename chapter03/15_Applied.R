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

# a ####

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
plot(x, y)

# x$coeffients <-  c('Intercept', "RME")

# b ####

# Fitting a multiple linear regression
mlr.fit = lm(crim ~ ., data=Boston)

# Viewing the statistics for the MLR model
summary(mlr.fit)

# c ####


# d ####


#library(ISLR)
library(MASS)

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
lm.function(rm)
lm.zn = lm(crim ~ zn, data = Boston)
lm.indus = lm(crim ~ indus, data = Boston)
lm.chas = lm(crim ~ chas, data = Boston)
lm.nox = lm(crim ~ nox, data = Boston)
lm.rm = lm(crim ~ rm, data = Boston)
lm.age = lm(crim ~ age, data = Boston)
lm.dis = lm(crim ~ dis, data = Boston)
lm.rad = lm(crim ~ rad, data = Boston)
lm.tax = lm(crim ~ tax, data = Boston)
lm.ptratio = lm(crim ~ ptratio, data = Boston)
lm.lstat = lm(crim ~ lstat, data = Boston)
lm.medv = lm(crim ~ medv, data = Boston)

summary(lm.zn)
par(mfrow = c(2,2)) # 4 plots in same picture
plot(lm.zn)
plot(lm.zn, Boston$zn)
summary(lm.indus)
summary(lm.chas)
summary(lm.nox)
summary(lm.rm)
summary(lm.age)
summary(lm.dis)
summary(lm.rad)
summary(lm.tax)
summary(lm.ptratio)
summary(lm.lstat)
summary(lm.medv)

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


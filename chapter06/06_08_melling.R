# Ch6 08

# a) ####
set.seed(112)
x <- rnorm(100)
epsilon <- rnorm(100)

# b) ####
beta0 = 1
beta1 = 1
beta2 = 1
beta3 = 1

Y <- beta0 + beta1 * x + beta2 * x^2 + beta3 * x^3 + epsilon

# c) ####
library(leaps)
regfit.full=regsubsets(Y~., poly(x, 10))
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq
reg.summary$bic
reg.summary$cp
reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$rsq)
plot(reg.summary$bic)
plot(reg.summary$cp)
plot(reg.summary$adjr2)

# Answer: polynomial 3

# d) ####

regfit.full=regsubsets(Y~., poly(x, 10), method = "forward")
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq
reg.summary$bic
reg.summary$cp
reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$rsq)
plot(reg.summary$bic)
plot(reg.summary$cp)
plot(reg.summary$adjr2)

regfit.full=regsubsets(Y~., poly(x, 10), method = "backward")
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq
reg.summary$bic
reg.summary$cp
reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$rsq)
plot(reg.summary$bic)
plot(reg.summary$cp)
plot(reg.summary$adjr2)

# Answer: Looks the same

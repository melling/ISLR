# Ch6 08 p262

# Other Solutions:
# https://rpubs.com/evertonjlima/242626
# https://rpubs.com/lmorgan95/ISLR_CH6_Solutions
# https://rstudio-pubs-static.s3.amazonaws.com/295803_0a2bb7a6e2e747cc81682d0c6fed787d.html
# https://blog.princehonest.com/stat-learning/ch6/8.html
# https://github.com/ppaquay/IntroStatLearning/blob/master/Chap6.Rmd


# a) ####
set.seed(112)
x <- rnorm(100)
epsilon <- rnorm(100)

# b) ####
beta0 = 1
beta1 = 1
beta2 = 1
beta3 = 1

beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3

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

# e) ####
## STUDY! ####

library(glmnet)
set.seed(1)

df = data.frame(x = x,
                y = Y)
# train = sample(1:nrow(df), nrow(df)*.7)

x = model.matrix(y ~ poly(x, 10, raw = T), data = df)[, -1]
mod.lasso = cv.glmnet(x, Y, alpha = 1)

best.lambda = mod.lasso$lambda.min
best.lambda

plot(mod.lasso)
full.model = glmnet(x, Y, alpha = 1)
predict(full.model, s = best.lambda, type = "coefficients")

#dim(train)
#dim(test)

#cv.out <- cv.glmnet(x[train], y[train], alpha=1)

# f) ####
beta7 = 7

x <- rnorm(100)
Y <- beta0 + beta7 * x^7 + epsilon

df = data.frame(y = Y, x = x)
                
View(df)
bss.fit <- regsubsets(y ~ poly(x, 10, raw = T), data = df, nvmax = 19)
summary(bss.fit)
par(mfrow=c(1,1))
plot(bss.fit, scale = "r2")
plot(bss.fit, scale = "adjr2")
plot(bss.fit, scale = "Cp")
plot(bss.fit, scale = "bic")

#  ---

reg.summary=summary(bss.fit)

names(reg.summary)
reg.summary$rsq
reg.summary$bic
reg.summary$cp
reg.summary$adjr2

par(mfrow=c(1,1))
plot(reg.summary$rsq)
plot(reg.summary$bic)
plot(reg.summary$cp)
plot(reg.summary$adjr2)

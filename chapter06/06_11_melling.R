# Chapter 6, Q11
# p264

#library(ISLR)
library(MASS)
library(leaps)

## 11a ####
### best subset selection ####

set.seed(1)

n = nrow(Boston)
dim(Boston)[2]-1
train = sample(1:n, n*.7)
#test = (-train)
bss.model <- regsubsets(crim ~ ., data = Boston[train,], method = "exhaustive", nvmax = dim(Boston)[2]-1)
summary.fit = summary(bss.model)

plot(summary.fit$bic) # lower is better
plot(summary.fit$cp) # lower is better
plot(summary.fit$rss) # lower is better
plot(summary.fit$adjr2) # lower is better

bic.min = which.min(summary.fit$bic)
bic.min
plot(summary.fit$bic) # lower is better
points(bic.min, summary.fit$bic[bic.min], col=2, cex=2, pch=20)

plot(bss.model)

### lasso ####

library(glmnet)

x = model.matrix(crim ~ ., data = Boston)[,-1] # Remove intercept
y = Boston$crim

grid = 10^seq(10,-2, length=100)
lasso.model = glmnet(x[train,], y[train], alpha = 1, lambda = grid) # , lambda = grid
plot(lasso.model)

cv.lasso = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.lasso)

best.lasso.lambda = cv.lasso$lambda.min

lasso.pred = predict(lasso.model, s=best.lasso.lambda, newx = x[-train,])

lasso.mse = mean((lasso.pred - y[-train])^2) # 23.30576

lasso.out = glmnet(x,y, lambda = grid) # , lambda = grid

lasso.coef = predict(lasso.out, type="coefficients", s=best.lasso.lambda)

lasso.coef

### ridge ####

ridge.model <- glmnet(x[train,], y[train], alpha = 0)
plot(ridge.model)

cv.ridge <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.ridge)

best.ridge.lambda = cv.ridge$lambda.min
which.min(cv.ridge$lambda) # Find index
cv.ridge$lambda[100]

cv.ridge$lambda.min 

ridge.predict = predict(ridge.model, s=best.ridge.lambda, newx = x[-train,])

ridge.mse = mean((ridge.predict - y[-train])^2)
ridge.mse; lasso.mse

### pcr ####

library(pls)

pcr.model = pcr(crim ~ ., data = Boston, subset = train, scale=TRUE, validation = "CV")

validationplot(pcr.model, val.type = "MSEP")

summary(pcr.model)

pcr.predict = predict(pcr.model, x[-train,], ncomp = 9)

pcr.mse = mean((pcr.predict - y[-train])^2)
ridge.mse; lasso.mse; pcr.mse

### pls ####

pls.model = plsr(crim ~ .,  data = Boston, subset = train, scale=TRUE, validation = "CV")

summary(pls.model)

pls.predict = predict(pls.model, x[-train,], ncomp = 9)

pls.mse = mean((pls.predict - y[-train])^2)
ridge.mse; lasso.mse; pcr.mse; pls.mse

## 11b ####


# Lasso is the best because MSE was the smallest


## 11c ####

# No Lasso removed some variables: age
# Saw others on a different run
# Got different results on a final run for some reason FIXME!

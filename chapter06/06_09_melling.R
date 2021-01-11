# Ch6 09 p263

library(ISLR)

# a) ####

set.seed(11)

sum(is.na(College))

training <- sample(1:nrow(College), nrow(College)*.7, replace = F)
dim(College)
train <- College[training,]
test <- College[-training,]

# b) ####
#View(College)
lm.fit = lm(Apps ~ ., data = train)

lm.predict <- predict(lm.fit, test)

mse.lm <- mean((lm.predict - test$Apps)^2)

# c) ####

library(glmnet)
x <- model.matrix(Apps ~ . , data = College )[,-1]
y <- College$Apps

grid = 10 ^ seq(4, -2, length=100)

ridge.model <- glmnet(x[training,], y[training], alpha = 0, lambda = grid, thresh = 1e-12)

# WARNING: Never forget , in x[training,]
cv.out <- cv.glmnet(x[training,], y[training], alpha=0)

plot(cv.out)

best_lambda <- cv.out$lambda.min
#rm(best_lamba)
ridge.pred <- predict(ridge.model, s=best_lambda, newx = x[-training,])

mse.ridge <- mean((ridge.pred - test$Apps)^2)

mse.lm - mse.ridge
mse.lm; mse.ridge

# d) ####

lasso.model <- glmnet(x[training,], y[training], alpha = 1, lambda = grid, thresh = 1e-12)
lasso.out <- cv.glmnet(x[training,], y[training], alpha=1)
best_lambda <- lasso.out$lambda.min

lasso.pred <- predict(lasso.model, s=best_lambda, newx = x[-training,])

mse.lasso <- mean((lasso.pred - test$Apps)^2)
mse.lasso
mse.lm; mse.ridge; mse.lasso

lasso.model <- glmnet(x[training,], y[training], alpha = 1, lambda = best_lambda)
predict(lasso.model, s=best_lambda, type="coefficients")

# e) ####

library(pls)

#set.seed(1)

pcr.fit = pcr(Apps ~ ., data = College, subset = training, scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, x[-training,], ncomp=5)

mse.pcr <- mean((pcr.pred - test$Apps)^2)
mse.pcr
mse.lm; mse.ridge; mse.lasso; mse.pcr

# f) PLS ####

pls.fit = plsr(Apps ~ ., data = College, subset = training, scale = TRUE, validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[-training,], ncomp=6)
mse.pls <- mean((pls.pred - test$Apps)^2)
mse.lm; mse.ridge; mse.lasso; mse.pcr;mse.pls

# g) PLS ####

# PCR was the worst by a lot
# Ridge was the best result

# Next time, try using R^2 to compare the models

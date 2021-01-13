#### 10 ####

library(leaps)

#### 10(a) ####

set.seed(1)

# Randomly generating X (1000 observations with 20 predictors)

p = 20
n = 1000

X <- matrix(ncol=p, nrow=n)

for(i in 1:n){
  X[i,] <- rnorm(p)
}

# Randomly generating Beta values with few being 0's
B = rnorm(p)

for (i in c(2, 5, 6, 7, 10, 15)) {
  B[i] = 0
}
  
# Randomly generating error terms
e = rnorm(1000)

# Creating the model

Y = X%*%B + e

# Creating a dataframe of X predictors and y response
df = data.frame(X, Y)

#### 10(b) ####

train = sample(x=nrow(df), size=100, replace=FALSE)
train_df = df[train,]
test_df = df[-train,]

#### 10(c) ####

# Fitting 
bss.fit = regsubsets(Y ~ ., data=train_df, nvmax=p)
bss.summary = summary(bss.fit)
bss.summary

plot(bss.summary$rss, xlab="Number of Variables", ylab="RSS")
min.rss = which.min(bss.summary$rss)
points(min.rss, bss.summary$rss[min.rss], col="red", cex=2, pch=20)

# Plotting the best variable model using BIC
plot(bss.fit, scale='bic')

train.mat = model.matrix(Y ~ ., data=train_df, nvmax=p)
val.errors = rep(NA, p)
for (i in 1:p) {
  
  # Calculates the coefficients of best i-variable model
  coefi = coef(bss.fit, id=i)
  
  # Multiplies the predictors with the corresponding coefficients 
  pred = train.mat[,names(coefi)] %*% coefi
  
  # Calculates the MSE and saves them in val.errors
  val.errors[i] = mean((pred - train_df$Y)^2)
}

# Plotting the MSE
plot(val.errors, 
     xlab="Number of Predictors", 
     ylab="Training MSE", 
     pch=15, 
     type="b")

#### 10(d) ###

# Running the trained model on the test data
test.mat = model.matrix(Y ~ ., data=test_df, nvmax=p)
test.errors = rep(NA, p)
for (i in 1:p) {
  
  # Calculates the coefficients of best i-variable model
  coefi = coef(bss.fit, id=i)
  
  # Multiplies the predictors with the corresponding coefficients 
  pred = test.mat[,names(coefi)] %*% coefi
  
  # Calculates the MSE and saves them in val.errors
  test.errors[i] = mean((pred - test_df$Y)^2)
}

# Plotting the MSE
plot(test.errors, 
     xlab="Number of Predictors", 
     ylab="Test MSE", 
     pch=15, 
     type="b")

#### 10(e) ####

# Number of predictors for lowest MSE on the training data
which.min(val.errors) # 20

# Number of predictors for lowest MSE on the test data
which.min(test.errors) # 14

#### 10(f) ####

coef(bss.fit, id=14)
# 2, 5, 6, 7 , 10, 15
# All the coefficients that was in the model showed up.

#### 10(g) ####

val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(bss.fit, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((B[colnames(test_df) %in% names(coefi)] - coefi[names(coefi) %in% colnames(test_df)])^2) + 
                sum(B[!(colnames(test_df) %in% names(coefi))])^2)
}
plot(x = a, y = b, xlab = "number of coefficients", ylab = "error between estimated and true coefficients")
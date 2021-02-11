#### 5(a) ####

# Generating the data with n = 500 and p = 2
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0) # Multiplying by 1 to turn it from bool to binary

#### 5(b) ####

# Plotting the data with class labels
plot(x1, x2, col=(4-y)) # Giving color to class (0->4, 1->3)

#### 5(c) ####

library(ISLR)

# Fitting a logistic regression
log.fit = glm(
  y ~ x1 + x2,
  family=binomial
)

#### 5(d) ####

# Calculating the predicted class using the logistic regression
log.probs = predict(
  log.fit,
  type="response"
)

# Converting probabilities into class predictions
log.pred = ifelse(log.probs>=0.5, 1, 0)

# Plotting the predicted boundary
plot(x1, x2, col=(4-log.pred))

' COMMENTS:
The logistic regression did a terrible job predicting the class
'

#### 5(e) ####

# Fitting a logistic regression using a non-linear function
poly.log.fit = glm(
  y ~ poly(x1,2) + poly(x2,2),
  family=binomial
)

#### 5(f) ####

# Predicting on the values
poly.log.probs = predict(
  poly.log.fit,
  type="response"
)

# Converting probs into predcition classes
poly.log.preds = ifelse(poly.log.probs>=0.5, 1, 0)

# Plotting the predicted boundary
plot(x1, x2, col=(4-poly.log.preds))

' COMMENTS:
Applying a non-linear function of x1, and x2 before fitting a 
logistic regression helped out a lot. The boundaries are looking similar 
to actual.
'

#### 5(g) ####

library(e1071)

# Converting into dataframe
df = data.frame(x1=x1, x2=x2, y=as.factor(y))

# Fitting a SVC
svc.tune = tune(
  svm,
  y ~ .,
  data=df,
  kernel="linear",
  ranges=list(cost=c(0.01, 0.1, 1, 10, 100, 200))
)

# Viewing the results
summary(svc.tune) # Best cost is 0.1

# Fitting a svc with the best cost parameter
svc.fit = svm(
  y ~ .,
  data=df,
  kernel="linear",
  cost=0.1
)

# Predicting using svc
svc.preds = predict(svc.fit, df)

# Plotting 
df.1 = df[svc.preds == 1, ]
df.0 = df[svc.preds == 0, ]
plot(df.1$x1, df.1$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(df.0$x1, df.0$x2, col = "red", pch = 4)

# Plotting with svc
plot(svc.fit, df)

#### 5(h) ####

# Fitting a svm
svm.fit = svm(
  y ~.,
  data=df,
  kernel="radial",
  gamma=1
)

# Predicting using svc
svm.preds = predict(svm.fit, df)

# Plotting 
df.1 = df[svm.preds == 1, ]
df.0 = df[svm.preds == 0, ]
plot(df.1$x1, df.1$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(df.0$x1, df.0$x2, col = "red", pch = 4)

' COMMENTS:
Based on the images, SVM with a radial kernel and a logistic regression
applied on non-linear functions did the best in predicting the classes.
'
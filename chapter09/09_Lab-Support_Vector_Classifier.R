#### 9.6 Lab: Support Vector Machines ####

library(e1071)

#### 9.6.1 Support Vector Classifier ####

# Setting the seed
set.seed(1)

# Creating X and y values
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1, 10), rep(1,10))

# Adding one to the last 10 rows in x
x[y==1,] = x[y==1,] + 1

# Plotting a scatter
plot(x, col=(3-y))

# Creating a dataframe
df = data.frame(x=x, y=as.factor(y))
' COMMENTS:
Need to turn reponse to factors to perform classification
'

# Fitting a SVM 
svm.fit = svm(
  y~.,
  data=df,
  kernel="linear", # fit a support vector classifier
  cost=10, # determines the violation of a margin 
  scale=FALSE # should svm() scale features
)

# Plotting svm
plot(svm.fit, df)

# Viewing the indices of the support vectors
svm.fit$index

# Viewing the summary of fit
summary(svm.fit)

# Fitting a svm with a smaller cost parameter
svm.fit.small = svm(
  y~.,
  data=df,
  kernel="linear", # fit a support vector classifier
  cost=0.1, # determines the violation of a margin 
  scale=FALSE # should svm() scale features
)

# Viewing the classifier
plot(svm.fit.small, df)

# Viewing the indices
svm.fit.small$index

# Cross validating to get optimal value for cost
tune.out = tune(
  svm,
  y~.,
  data=df,
  kernel="linear",
  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100))
)

# Viewing the results of tune()
summary(tune.out)
' COMMENTS:
Best error (0.10) is when cost is 5 or 10
'

# Getting the best model from tune()
bestmod = tune.out$best.model
summary(bestmod)

# Predicting

## Creating test set
xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdata = data.frame(x=xtest, y=as.factor(ytest))

## Predicting on new data
ypred = predict(bestmod, testdata)

## Creating confusion matrix
table(predict=ypred, truth=testdata$y)

# Fitting svm with cost 0.01
svmfit01 = svm(
  y~.,
  data=df,
  kernel='linear',
  cost=0.01,
  scale=FALSE
)

ypred = predict(svmfit01, testdata)
table(predict=ypred, truth=testdata$y)

# Applying svm on a barely linearly separable observations
x[y==1,] = x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)

# Creating new data
data = data.frame(x=x, y=as.factor(y))

# Fitting svm
svmfit = svm(
  y~.,
  data=data,
  kernel="linear",
  cost=1e5
)

# Viewing the summary of fit
summary(svmfit)

# Plotting the margin classifier
plot(svmfit, data)
' COMMENTS:
Margin is very narrow because observations, that are NOT support vectors,
are very close to boundary
'

svmfit1 = svm(
  y~.,
  data=data,
  kernel="linear",
  cost=1
)

summary(svmfit1)

plot(svmfit1, data)

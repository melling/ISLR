#### 9.6.5 Application to Gene Expression ####

# Libraries
library(ISLR)
library(e1071)

# Viewing the Khan dataset (Gene Expression)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# Viewing the number of observations in each class in train data
table(Khan$ytrain)

# Viewing the number of observations in each class in test data
table(Khan$ytest)

' COMMENTS:
very large number of features relative to the number of observations
'

# Creating dataframe for train
df.train = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))

# Fitting svm()
svm.fit = svm(
  y~.,
  data=df.train,
  kernel="linear",
  cost=10
)

# Viewing the stats of fit
summary(svm.fit)

# Viewing the results
table(svm.fit$fitted, df.train$y)

' COMMENTS:
No training errors. Large number of variables relative to observations 
implies that finding hyperplane that fully separate classes is easy.
'

# Creating the test dataframe
df.test = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))

# Predicting on the test data
preds = predict(svm.fit, newdata=df.test)

# Creating confusion matrix
table(preds, df.test$y)

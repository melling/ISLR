#### 9.6.2 Support Vector Machine ####

# Library
library(e1071)

# Setting the seed
set.seed(1)

# Creating X data
x = matrix(rnorm(200*2), ncol=2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2

# Creating the y
y = c(rep(1,150), rep(2,50))

# Creating dataframe with x and y
df = data.frame(x=x, y=as.factor(y))

# Plotting dataframe
plot(x, col=y) # Setting the color based on the labels

# Train boolean vector
train = sample(200, 100)

# Fitting svm()
svm.fit = svm(
  y~.,
  data=df[train,],
  kernel="radial",
  gamma=1,
  cost=1
)

# Viewing the summary
summary(svm.fit)

# Fitting svm() with bigger cost
svm.fit = svm(
  y~.,
  data=df[train,],
  kernel="radial",
  gamma=1,
  cost=1e5
)

# Plotting svm
plot(svm.fit, df[train,])

# Using tune() to apply CV to determine gamma and cost
tune.out = tune(
  svm,
  y~.,
  data=df[train,],
  kernel="radial",
  ranges=list(cost=c(0.1, 1, 10, 100, 1000), 
              gamma=c(0.5, 1, 2, 3, 4))
)

# Viewing the results
summary(tune.out)

' COMMENTS: 
Best combination is cost=1 and gamma=0.5.
'

# Predicting with the best parameters of svm
preds = predict(tune.out$best.model, newdata=df[-train,])

# Creating confusion matrix
table(true=df[-train, "y"], pred=preds)

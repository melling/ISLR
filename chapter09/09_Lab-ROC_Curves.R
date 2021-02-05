#### 9.6.3 ROC Curves ####

library(ROCR)

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

# Function to plot ROC curve
rocplot = function(pred, truth, ...) {
  
  # Formating the predicted and response values for ROC curves
  predob = prediction(pred, truth)
  
  # Evaluating the accuracy of predictions
  perf = performance(predob, "tpr", "fpr")
  
  # Plotting the curves
  plot(perf, ...)
}

' COMMENTS:
SVMs can output fitted values for each observation (numerical scores to 
determine what class). The sign of fitted value determines which side of 
the decision boundary the observation
'

# Fitting svm
svm.fit.opt = svm(
  y~., 
  data=df[train,],
  kernel="radial",
  gamma=2,
  cost=1,
  decision.values=TRUE
)

# Predicting on the train data
preds = predict(
  svm.fit.opt, 
  df[train,], 
  decision.values=TRUE
)

fitted = attributes(preds)$decision.values

# Plotting ROC curve
par(mfrow=c(1,2))
rocplot(fitted, df[train, "y"], main="Training Data")

# Fitting a more flexible model by increasing gamma
svm.fit.flex = svm(
  y~.,
  data=df[train,],
  kernel="radial",
  gamm=50,
  cost=1,
  decision.values=TRUE
)

# Predicting on the train data
preds = predict(
  svm.fit.flex, 
  df[train,], 
  decision.values=TRUE
)

fitted = attributes(preds)$decision.values

rocplot(fitted, df[train, "y"], add=TRUE, col="red")

# Predicting in the test data
preds_test = predict(
  svm.fit.opt,
  df[-train,],
  decision.values=TRUE
)

fitted_test = attributes(preds_test)$decision.values

# Plotting
rocplot(fitted_test, df[-train,"y"], main="Test Data")

# Predicting in the test data
preds_test = predict(
  svm.fit.flex,
  df[-train,],
  decision.values=TRUE
)

fitted_test = attributes(preds_test)$decision.values

# Plotting
rocplot(fitted_test, df[-train,"y"], add=TRUE, col="red")


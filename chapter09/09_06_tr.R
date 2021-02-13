#### 6(a) ####

# Generating dataset with n = 1000, p = 2
x1 = runif(1000) - 0.5
x2 = runif(1000) - 0.5
y = 1 * (0.4 - log(x1+1.5) + x2 > 0)

# Creating dataframe of x1, x2, y
df = data.frame(x1, x2, y)

# Plotting x1 and x2 with class labels as colors
plot(df$x1, df$x2, col=(4-df$y))

#### 6(b) ####

# Library
library(e1071)

costs = c(0.001, 0.01, 0.1, 1, 100)

# Cross validating on the data
cv.svm = tune(
  svm,
  as.factor(y) ~.,
  data=df,
  kernel="linear",
  ranges=list(cost=costs)
)

# View the results of the cross validation
summary(cv.svm)

# Plot the errors per cost
plot(cv.svm)

# Function to get accuracy for every cost used in the cross validation
confusion.matrix = function(dataframe, costs) {
  errors = rep(0, length(costs)) 
  for (i in 1:length(costs)) {                      # For every cost 
    fit = svm(                                      # Fit a svc
      as.factor(y) ~.,
      data=df,
      kernel="linear",
      cost=costs[i]
    )
    fit.preds = predict(fit, newdata=dataframe)           # Predict on data
    result = table(preds=fit.preds, true=dataframe$y)     # Create confusion matrix
    accuracy = (result[1]+result[4]) / sum(result) # Get accuracy
    errors[i] = accuracy                           # Save accuracy in list
  }
  return(errors)                                   # Return list
}

# Getting the errors per cost
cost.training.errors = confusion.matrix(df, costs)
cost.training.errors

' COMMENTS:
The training errors complement the results of the cross-validation results.
'

# Generating test with n = 200, p = 2
t1 = runif(400) - 0.5
t2 = runif(400) - 0.5
ty = 1 * (0.4 - log(t1+1.3) + t2 > 0)

test = data.frame(x1=t1, x2=t2, y=ty)

## Creating confusion matrix
test.errors = confusion.matrix(test, costs)
test.errors

' COMMENTS:
Based on the errors, the smaller cost gave a better result on the test data 
as well.
'
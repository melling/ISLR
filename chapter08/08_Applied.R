#### 08 Applied ####

# Imports
library(ISLR)
library(randomForest)
library(tree)

#### 8(a) ####

# Setting seed
set.seed(1)

# Saving number of rows in a variable
num.rows = nrow(Carseats)

# Sample of indices for training (70/30 split)
train = sample(1:num.rows, num.rows*0.7)

#### 8(b) ####

# Fitting a decision tree with default parameters
tree.fit = tree(
  Sales ~ .,
  data=Carseats,
  subset=train
)

# Viewing the summary
tree.fit

# Plotting the decision tree
plot(tree.fit)
text(tree.fit, pretty=0)
' COMMENTS:
18 terminal nodes
'

# Predicting on test set
yhat.dt = predict(
  tree.fit, # The trained model
  newdata=Carseats[-train,] # The unseen data (test)
)

# Calculating test MSE
mean((yhat.dt - Carseats[-train,]$Sales)^2) 

# A decision tree gets a test MSE of 4.208383.

#### 8(c) ####

# Applying CV to determine the optimal terminal nodes
cv.carseats = cv.tree(tree.fit)

# Viewing the cv results
cv.carseats

# Plotting the deviance
plot(cv.carseats$size, 
     cv.carseats$dev, 
     type="b", 
     xlab="Terminal Nodes", 
     ylab="Deviance")

' COMMENTS:
Based on this graph, Deviance levels off around 8-10 nodes
'

# Pruning the tree based on 9 nodes
prune.carseats = prune.tree(tree.fit, best=9)

# Viewing the pruned tree
plot(prune.carseats)
text(prune.carseats, pretty=0)

# Predicting on the test set
yhat.pruned = predict(prune.carseats, newdata=Carseats[-train,])

# Calculated the MSE
mean((yhat.pruned - Carseats[-train,]$Sales)^2) # 4.469155 

' COMMENTS: 
The MSE slightly increased by pruning the tree
'

#### 8(d) ####

bag.carseats = randomForest(
  Sales ~.,
  data=Carseats,
  subset=train,
  mtry=dim(Carseats)[2]-1,
  importance=TRUE
)

# Viewing the results of the bagging
bag.carseats

# Predicting using bagging on test data
yhat.bag = predict(bag.carseats, newdata=Carseats[-train,])

# Calculating MSE
mean((yhat.bag - Carseats[-train,]$Sales)^2) # MSE: 2.540992

# Viewing the variable importance
importance(bag.carseats)
varImpPlot(bag.carseats)
' COMMENTS:
Based on the varImpPlot, "Price", "ShelvLoc", "CompPrice", "Age", and
"Advertising" are key variables to predict "Sales".  
'

#### 8(e) ####

# Fitting a random forest
rf.carseats = randomForest(
  Sales~.,
  data=Carseats,
  subset=train,
  importance=TRUE,
  mtry=6
)

# Viewing the results of the random forest
rf.carseats

# Predicting the test data using the random forest
yhat.rf = predict(rf.carseats, newdata=Carseats[-train,])

# Calculating the MSE
mean((yhat.rf - Carseats[-train,]$Sales)^2) # MSE: 2.536241

# Viewing the variable importance
importance(rf.carseats)

# Plotting Variance Importance
varImpPlot(rf.carseats)

' COMMENTS:
It seems like the more predictors we add, the test MSE gets smaller. 
'

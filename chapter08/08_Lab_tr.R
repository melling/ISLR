#### 8.3 LAB: Decision Trees ####

#### 8.3.1 Fitting Classification Trees ####

# Imports
library(tree)
library(ISLR)

# Creating a binary variable
High = as.factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))

# Adding "High" into Carseats df
Carseats = data.frame(Carseats, High)

# Fitting a tree
tree.carseats = tree(High~.-Sales, data=Carseats)

# Viewing the summary
summary(tree.carseats)

# Plotting the decision tree
plot(tree.carseats)
text(tree.carseats, pretty=0) # Labeling the tree plot

# Viewing the criterion for each split
tree.carseats

### Splitting Data into Train and Test ###

# Setting the seed
set.seed(2) 

# Sampling 200 indices from data for train
train = sample(1:nrow(Carseats), 200)

# Setting the test data as -train indices
Carseats.test = Carseats[-train,]
High.test = High[-train]

# Fitting a tree model
tree.carseats = tree(High ~ . -Sales, data=Carseats, subset=train)

# Predicting the test data using the trained model
tree.pred = predict(tree.carseats, Carseats.test, type='class')

# Creating a confusion matrix
table(tree.pred, High.test) # Accuracy: 0.77

### Cross Validation and Pruning ###

# Setting the seed
set.seed(3)

# Using CV to determine the best pruned tree
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
'COMMENTS:
Use prune.misclass to use classifcation error rate as the metric
as opposed to cv.tree() default, deviance
'

# Viewing the statistics in a cv.tree()
names(cv.carseats)

# Viewing the results of cv.tree()
cv.carseats
'COMMENTS:
size: number of terminal nodes
dev: metric used (this case is corresponding error rate)
k: ost-complexity parameter (alpha)

Best pruned tree is tree with 9 terminal nodes (size) with a error
rate of 66 (dev) and an alpha value of 1.4 (k)
'

# Plotting error rate
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# Create a tree model with best number of nodes from pruning
prune.carseats = prune.misclass(tree.carseats, best=9)

# Plotting pruned decision tree
plot(prune.carseats)
text(prune.carseats, pretty=0) # Adding text to the tree

# Predicting the test data
tree.pred = predict(prune.carseats, Carseats.test, type='class')

# Confusion matrix
table(tree.pred, High.test) # Accuracy: 0.775

#### 8.3.2 Fitting Regression Trees ####

# Imports
library(ISLR)
library(MASS)

# Setting seed
set.seed(1)

# Creating a boolean vector for train indices
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Fitting a random forest on the train data
tree.boston = tree(medv ~ ., data=Boston, subset=train)

# Summary of tree
summary(tree.boston)
'COMMENTS:
Summary stats indicate only four predictors are used to construct
the tree: "rm", "lstat", "crim", "age"
'

# Plottng the tree
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston, pretty=0) # Adding text to DT

# Find the best number of terminal nodes using CV
cv.boston = cv.tree(tree.boston)

# Plot the deviance for each number of terminal nodes
plot(cv.boston$size, cv.boston$dev, type="b")
'
Best number of terminal nodes is 6
'

# Pruning tree based on 6 terminal nodes
prune.boston = prune.tree(tree.boston, best=6)

## Plotting the pruned decision tree
plot(prune.boston)
text(prune.boston, pretty=0)

# Predicting on test data using unpruned tree
yhat = predict(tree.boston, newdata=Boston[-train,])

# Getting the response values from test data
boston.test = Boston[-train, "medv"]

# Plotting
plot(yhat, boston.test)

# Creating a straight line with intercept 0 and slope 1
abline(0,1)

# Calculating the MSE
mean((yhat-boston.test)^2)

#### 8.3.3 Bagging and Random Forests ####

# Imports
library(randomForest)

# Setting the seed
set.seed(1)

# Fitting a bagging model
bag.boston = randomForest(medv ~ ., data=Boston, subset=train,
                          mtry=13, importance=TRUE)
'COMMENTS:
mtry: number of predictors used for random forest
If all predictors are used, then it becomes bagging.
'

# Viewing the bagging stats
bag.boston

# Predicting using the bagging model
yhat.bag = predict(bag.boston, newdata=Boston[-train,])

# Plotting
plot(yhat.bag, boston.test)
abline(0,1) # Creating straight line starting at 0 and slope of 1

# Calculating test MSE
mean((yhat.bag-boston.test)^2) # MSE: 23.59273

### Random Forest model ###

# Setting the seed
set.seed(1)

# Fitting a randomforest with 6 predictors
rf.boston = randomForest(medv ~ ., data=Boston, subset=train,
                         mtry=6, importance=TRUE)

# Predicting test data with rf
yhat.rf = predict(rf.boston, newdata=Boston[-train,])

# Calculating the MSE
mean((yhat.rf-boston.test)^2) # MSE: 19.62021

# Viewing the significance of each predictor
importance(rf.boston)
'COMMENTS:

%IncMSE: mean decrease of accuracy in predictions on OOB samples 
when a given predictor is excluded from model

IncNodePurity: total decrease in node impurity  that results from
splits over a predictor, averaged over all trees
'

# Plotting the Variable Importance
varImpPlot(rf.boston)

#### 8.3.4 Boosting ####

# Imports
library(gbm)

# Setting the seed
set.seed(1)

# Fitting a boosting model
boost.boston = gbm(medv ~., data=Boston[train,], 
                   distribution="gaussian", n.trees=5000,
                   interaction.depth=4)
'COMMENTS: 
interaction.depth: the depth of each tree
distribution: "gaussian" -> regression; 
              "bernoulli" -> classification 
'

# Viewing the summary stats
summary(boost.boston)

# Plotting partial dependence plots
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
'COMMENTS:
Partial Dependence plots illustrates marginal effect of the 
selected variables on the response after integrating out the other
variables.
'

# Predicting the test data using the boosted model
yhat.boost = predict(boost.boston, newdata=Boston[-train,],
                     n.trees=5000)

# Calculating MSE
mean((yhat.boost-boston.test)^2) # MSE: 18.84709

# Fitting another boosting model with a different shrinkage 
# parameter
boost.boston = gbm(medv ~ ., data=Boston[train,],
                   distribution="gaussian", 
                   n.trees=5000,
                   interaction.depth=4, 
                   shrinkage=0.2, 
                   verbose=F)

# Predicting on the test data
yhat.boost = predict(boost.boston, 
                     newdata=Boston[-train,], 
                     n.trees=5000)

# Calculating MSE
mean((yhat.boost-boston.test)^2) # MSE: 18.33455

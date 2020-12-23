library(ISLR)

#### 10(a) ####

# Viewing the column names
names(Weekly)

# Viewing the dimensions
dim(Weekly)

# Graphically seeing the pairs
pairs(Weekly)

#### 10(b) ####

# Fitting the data using Logistic Regression
lr.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly,
             family=binomial)

# Viewing the summary statistics of the LR model
summary(lr.fit)
## Only Lag2 seems to be the only one that is significant

# Predicting total dataset
lr.prob = predict(lr.fit, type="response")

# Creating a confusion matrix

## Creating a placehodler vector with value "Down"
lr.preds = rep("Down", nrow(Weekly))

## Convert "Down" to "up if predicted probabilities is > 0.5
lr.preds[lr.prob > 0.5] = "Up"

## Generating a confusion matrix using table()
table(lr.preds, Weekly$Direction)

# Based from the confusion matrix:
(54+557)/nrow(Weekly) # Number of correct predictions: 56.1%
(430+48)/nrow(Weekly) # training error rate: 43.9%
(430)/(54+430) # Errors when market goes down: 88.8%

## Based on the confusion matrix, the LR model is having a difficult time predicting 
## when down the market is going down. It fails to predict accurately 88% of the time 
## when the market is going down.

#### 10(d) ####

# Creating a boolean vector for data that is going to be used for training
train = (Weekly$Year < 2009)

# Getting the test data (data that is labeled false in the train boolean)
Weekly.2009 = Weekly[!train, ]

# Extracting the responses from the test data
Direction.2009 = Weekly$Year[!train]

# Fitting the model using only Lag2 (the only significant coefficient)
lr.lag2.fit = glm(Direction ~ Lag2, data=Weekly, family=binomial, subset=train)

# Predicting the probabilities using the test data
lr.lag2.probs = predict(lr.lag2.fit, Weekly.2009, type="response")

# Creating confusion matrix
lr.lag2.preds = rep("Down", nrow(Weekly.2009)) # placeholder for predictions
lr.lag2.preds[lr.lag2.probs > 0.5] = "Up" # Setting "Up" for probability > 0,5
table(lr.lag2.preds, Direction.2009) # Confusion matrix of predictions and actual

# Calculating percentage of correct predictions
(8+46)/(nrow(Weekly.2009)) # 51.9%


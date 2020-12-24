#### 4.6.1 The Stock Market Data ####

# Importing libraries
library(ISLR)

# Viewing the column names in Smarket
names(Smarket)

# Viewing the dimensions of Smarket
dim(Smarket)

# Viewing the summary statistics of Smarket
summary(Smarket)

# Viewing correlation between the numeric features
cor(Smarket[,-9])
## Volume seems to have a good correlation with year

# Plotting Volume 
plot(Smarket$Volume)

#### 4.6.2 Logisitic Regression ####

# Fitting a Generalized Linear Model (set family = binomial for logistic regression)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, 
              family=binomial)
summary(glm.fit)

# Extracting only the coefficients from model
coef(glm.fit)

# Extracting only the coefficients from summary of model
summary(glm.fit)$coef

# Extracting only the p-values from summary of model
summary(glm.fit)$coef[,4]

# Predicting the probability that market will go up
# type = 'response' tells R to output probabilities as P(Y=1|X)
glm.probs = predict(glm.fit, type="response")

# Viewing the first ten probabilities
glm.probs[1:10]

# Using contrast to see dummy variables are associated with the actual variables
contrasts(Smarket$Direction)

# Converting the predicted probabilities into class labels: Up and Down
## Creating a placeholder vector of "Down" with the length of number of rows of data 
glm.pred = rep("Down", dim(Smarket)[1])

## Transforms "Down" to "Up" if the prediction probabilities is greater than 0.5
glm.pred[glm.probs > .5] = "Up"

# Generating a confusion matrix using table()
table(glm.pred, Smarket$Direction)

# Calculating the percentage of correct predictions
mean(glm.pred == Smarket$Direction)

# Splitting data into train and test set by subsettting by Year
## Creating a boolean vector that labels an observations TRUE if it is training data
train = (Smarket$Year < 2005)

## Getting the test data (data that is NOT train)
Smarket.2005 = Smarket[!train, ]

# Viewing the dimensions of test data
dim(Smarket.2005)

# Getting the response of the test data
Direction.2005 = Smarket$Direction[!train]

# Fitting data on traininig data
glm.train.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket,
                    family=binomial, subset=train)

# Predicting the test data using the glm model
glm.test.probs = predict(glm.train.fit, Smarket.2005, type="response")

# Converting the predicted probabilities into class labels: Up and Down
## Creating a placeholder vector of "Down" with the length of number of rows of data 
glm.test.pred = rep("Down", dim(Smarket.2005)[1])

## Transforms "Down" to "Up" if the prediction probabilities is greater than 0.5
glm.test.pred[glm.test.probs > .5] = "Up"

# Generating a confusion matrix using table()
table(glm.test.pred, Direction.2005)

# Calculating test error rate
mean(glm.test.pred != Direction.2005)

# Fitting a glm on Lag 1 and Lag 2 only
glm.lag12.fit = glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial, subset=train)

# Predicting on test set
glm.lag12.probs = predict(glm.lag12.fit, Smarket.2005, type="response")

# Creating a prediction vector
glm.lag12.pred = rep("Down", dim(Smarket.2005)[1])
glm.lag12.pred[glm.lag12.probs > .5] = "Up"
table(glm.lag12.pred, Direction.2005)

# Calculating test error rate
mean(glm.lag12.pred != Direction.2005)

#### 4.6.3 Linear Discriminant Analysis ####

library(MASS)

# Fitting a LDA on Smarket data
lda.fit = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit

## pihat1 = 0.492 which means that 49.2% of training data corresponds to "Down"

# Predicting values on test data
lda.pred = predict(lda.fit, Smarket.2005)
## Returns:
## class -> LDA predictions
## posterior -> matrix whose kth column contains the posterior probability that the 
## corresponding observation belongs to the kth class
## x -> contains the linear discriminants

# Viewing the three elements returned by predict(LDA)
names(lda.pred)

# Creating a confusion matrix
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) # 0.5595

sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

#### 4.6.4 Quadratic Discriminant Analysis ####

# Fitting QDA on Smarket
qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

# Predicting test data using QDA
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # 0.5992

## Comments:
## The QDA prediction is higher than LDA on TEST data which might suggest that the true
## relationship of the response and predictors is a quadratic form

#### 4.6.5 K-Nearest Neighbors ####

# Importing libraries
library(class)

# Setting seed
set.seed(1)

# Binding Lag1 and Lag2 of Smarket data for training set
train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
# Creating response for training set
train.Direction = Smarket$Direction[train]

# Binding Lag1 and Lag2 of Smarket for test set
test.X = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]

# Fitting data using KNN model
knn.pred = knn(train.X, test.X, train.Direction, k=1)

# Creating a confusion matrix
table(knn.pred, Direction.2005)
(83+43)/length(Direction.2005) # Percentage of correct predictions: 50%

# Using KNN model with K=3
knn3.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn3.pred, Direction.2005)
(48+87)/length(Direction.2005) # Percentage of correct predictions: 53.6%

#### 4.6.6 An Application to Caravan Insurance Data ####

# Viewing the dimensions of dataset
dim(Caravan)

# Viewing the summary of the response variable, Purchase
summary(Caravan$Purchase)

# Checking the number of Yes over total
348/5882 # 0.05912

# Standardizing data, except the response variable, to be on a comparable scale
# standardizing : std = 1 and mean = 0
standardized.X = scale(Caravan[,-86])

# Variance of column 1 and 2 before standardizing
var(Caravan[,1]) # 165
var(Caravan[,2]) # 0.164

# Variance after standardizing
var(standardized.X[,1]) # 1
var(standardized.X[,1]) # 1

# Splitting data into train and test sets
# creating boolean for test data
test = 1:1000

# Getting train data (predictors and response)
train.X = standardized.X[-test,]
train.Y = Caravan$Purchase[-test]

# Getting test data
test.X = standardized.X[test,]
test.Y = Caravan$Purchase[test]

# Setting the seed for KNN
set.seed(1)

# Fitting data using KNN with k=1
knn.pred = knn(train.X, test.X, train.Y, k=1)

# Checking accuracy
mean(test.Y != knn.pred) # 0.114

# Viewing how many test data is 
mean(test.Y != "No") # 0.059

## Comments:
## If people randomly guess who purchases insurance, there will be a 6% success rate

# Creating confusion matrix
table(knn.pred, test.Y)

## Comments:
## Out of the 77 people KNN predicting will buy insurance, 9 people (11.7%) will 
## purchase insurance. 

# Fitting data using KNN with k=3
knn.3.pred = knn(train.X, test.X, train.Y, k=3)

# Creating confusion matrix
table(knn.3.pred, test.Y)

# Percent of times KNN predicting 'Yes' correctly
5/26 # 19.2%

# Fitting data using KNN with k=5
knn.5.fit = knn(train.X, test.X, train.Y, k=5)

# Creating confusion matrix
table(knn.5.fit, test.Y)

# Percent of times KNN predicting 'Yes' correctly
4/15 # 26.7%

# Fitting the data using Logistic Regression
glm.fit = glm(Purchase ~ ., data=Caravan ,family=binomial, subset=-test)

# Calculating the probabilities of Purchase
glm.probs = predict(glm.fit, Caravan[test,], type='response')

# Creating predictions using threshold of 0.5
# Creating a placeholder vector of "No" 
glm.pred = rep("No", length(test)) 

# Using threshold to change "No" to "Yes"
glm.pred[glm.probs > .5] = "Yes"

# Creating a confusion matrix
table(glm.pred, test.Y)

# Changing threshold from 0.5 to 0.25
glm.pred.25 = rep("No", length(test))
glm.pred.25[glm.probs > 0.25] = "Yes"
table(glm.pred.25, test.Y)

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

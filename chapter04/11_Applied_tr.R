#### 11 ####

# Importing libraries
library(ISLR)
library(MASS)

# Viewing the column names
names(Auto)

#### 11(a) ####

# Creating a mpg01 predictor that returns 1 if greater than median of mpg
# Creating a vector that returns 1 if >= median(Auto$mpg) else 0
mpg01 = rep(0, nrow(Auto))
mpg01[Auto$mpg >= median(Auto$mpg)] = 1

# Creating a new dataframe combining Auto and mpg01
new.auto = data.frame(Auto, mpg01)


#### 11(b) ####

# Graphically viewing the correlation of mpg01 with other predictors
pairs(new.auto)

## Comments: There seems to be some association with horsepower, weight, acceleration, 
## and possibly displacement. There is a distinction between values when mpg01 = 1
## or mpg01 = 0.

# Boxplots of mpg01 by displacement, acceleration, horsepower, weight
boxplot(displacement~mpg01,data=new.auto, xlab="MPG > Median", ylab="Displacement")
boxplot(acceleration~mpg01,data=new.auto, xlab="MPG > Median", ylab="Acceleration")
boxplot(horsepower~mpg01,data=new.auto, xlab="MPG > Median", ylab="Horsepower")
boxplot(weight~mpg01,data=new.auto, xlab="MPG > Median", ylab="Weight")

## Comments: Based on the bosplots, acceleration might be hard to predict if mpg is 
## higher than median. The values of acceleration are too close to tell apart.

#### 11(c) ####

# Setting seed
set.seed(1)

# Creating sample size for train data (80% of the data)
sample.size = floor(0.8*nrow(new.auto))

# Get random sample of indices for train data
train = sample(seq_len(nrow(new.auto)), size=sample.size)

# Creating the train and test data
train.data = new.auto[train,]
test.data = new.auto[-train, ]

# Creating responses for train and test
y.train = train.data$mpg01
y.test = test.data$mpg01

#### 11(d) ####

# Fitting data using LDA
lda.fit = lda(mpg01 ~ displacement + horsepower + weight, data=new.auto, subset = train)

# Predicting mpg01 using the test data
lda.pred = predict(lda.fit, test.data)

# Generating a confusion matrix
lda.class = lda.pred$class
table(lda.class, y.test) 

# Calculating the test error rate
mean(lda.class != y.test) # test error rate: 12.7%

#### 11(e) ####

# Fitting data using QDA
qda.fit = qda(mpg01 ~ displacement + horsepower + weight, data=new.auto, subset = train)

# Predicting mpg01 using the test data
qda.pred = predict(qda.fit, test.data)

# Creating a confusion matrix
qda.class = qda.pred$class
table(qda.class, y.test)

# Calculating test error rate
mean(qda.class != y.test) # test-error rate: 10.1%

#### 11(f) ####

# Fitting data using LR
lr.fit = glm(mpg01 ~ displacement + horsepower + weight, 
             data=new.auto, 
             subset = train,
             family=binomial)

# Predicting on test set
lr.fit.probs = predict(lr.fit, test.data, type="response")

# Creating a prediction vector
lr.fit.pred = rep(0, nrow(test.data))
lr.fit.pred[lr.fit.probs > .5] = 1
table(lr.fit.pred, y.test)

# Calculating test error rate
mean(lr.fit.pred != y.test) # test-error rate: 6.33%

#### 11(g) ####

# Getting data readty for KNN

## Extracting the predictors from training data
train.X = cbind(train.data$displacement + train.data$horsepower + train.data$weight)

## Extracting the response from training data
train.y = train.data$mpg01

## Extracting predictors from test set
test.X = cbind(test.data$displacement + test.data$horsepower + test.data$weight)

## Scaling the data
scaled.train.X = scale(train.X)
scaled.test.X = scale(test.X)

# Fitting data using KNN with k=1
knn.pred.1 = knn(scaled.train.X, scaled.test.X, train.y, k=1)

# Calculating test error rate
mean(y.test != knn.pred.1) # 15.1% (changed from 13.9% after scaling)

# Fitting data using KNN with k=3
knn.pred.3 = knn(scaled.train.X, scaled.test.X, train.y, k=3)

# Calculating test error rate
mean(y.test != knn.pred.3) # 12.7% (changed from 13.9% after scaling)

# Fitting data using KNN with k=5
knn.pred.5 = knn(scaled.train.X, scaled.test.X, train.y, k=5)

# Calculating test error rate
mean(y.test != knn.pred.5) # 10.1%

# Fitting data using KNN with k=10
knn.pred.10 = knn(scaled.train.X, scaled.test.X, train.y, k=10)

# Calculating test error rate
mean(y.test != knn.pred.10) # 10.1%

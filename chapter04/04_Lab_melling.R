## Chapter 4 Lab p 155
library(ISLR)

### Stock Market Data ####

names(Smarket)
dim(Smarket)

summary(Smarket)

cor(Smarket[,-9]) # Skip col 9 because it's qualitative

attach(Smarket)
#detach(Smarket)
plot(Volume)

### Logistic Regression p156 ####

glm.fit1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)  # binomial for logistic regression
summary(glm.fit1)              

coef(glm.fit1)

summary(glm.fit1)$coef
summary(glm.fit1)$coef[,4] # Column 4

# p157
# P(Y=1|X)
# If no data set is supplied to the predict() function, then the probabilities are computed for the training data that was used to fit the logistic regression mode
glm.probs = predict(glm.fit1, type = "response")
glm.probs[1:10]
#View(glm.probs)

contrasts(Smarket$Direction)

### Confusion Matrix ####
# Create prediction table
glm.probs
glm.pred = rep("Down", 1250) 
glm.pred[glm.probs > .5] = "Up"
summary(glm.pred)
# Produce a confusion matrix
# Diagonals indicate correct predictions. Off-diagonals indicate incorrect predictions.
table(glm.pred, Smarket$Direction)

# Determine how many correctly and incorrectly classified
(507 + 145) / 1250 # Correct 52.16%

mean(glm.pred == Smarket$Direction)
glm.pred

train = (Smarket$Year<2005)
train[1:20]
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket$Direction[!train]
Smarket.2005[1:10,]
head(Smarket.2005)

### Fit Logistic Regression Model ####

glm.fit2 = glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs2 = predict(glm.fit2, Smarket.2005, type = "response")

# Now check prediction

glm.pred = rep("Down", 252)
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) # Compute and test error rate

### Remove Predictors ####

# remove +Lag3+Lag4+Lag5+Volume
glm.fit1 = glm(Direction ~Lag1+Lag2, data = Smarket, family = binomial, subset = train) # Train Data
glm.probs2 = predict(glm.fit1, Smarket.2005, type = "response")

# Again check

glm.pred = rep("Down", 252)
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
106 / (106 + 76)

predict(glm.fit1, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type = "response")

## Linear Discriminant Analysis ####

library(MASS)

# Same format as glm: glm(Direction ~Lag1+Lag2, data = Smarket, family = binomial, subset = train) - family
lda.fit = lda(Direction ~Lag1+Lag2, data = Smarket, subset = train)
lda.fit

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)

mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >=.5)
sum(lda.pred$posterior[,1] <.5)

sum(lda.pred$posterior[,1] >.9) # Want only over 90% posterior probability
# 0 !!!

## Quadratic Discriminant Analysis ####
# Same format as lda()

qda.fit = qda(Direction ~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.pred = predict(qda.fit, Smarket.2005)
names(qda.pred)

qda.class = qda.pred$class
table(qda.class,Direction.2005)

mean(qda.class == Direction.2005) # Accurate 60% of the time

## K-Nearest Neighbors ####

#data(package="ISLR")
# knn() requires 4 inputs
# 1. matrix of predictors for training data: train.X
# 2. matrix of predictors for test.X
# 3. vector containing class labels for training observations: train.Direction
# 4. value for K, number of nearest neighbors to be used by classifier

library(class)

train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
#View(train)
#train.X = cbind(Lag2)[train,]

#View(train.X)
test.X = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train.Direction = Smarket$Direction[train]
#train.Direction
#Direction[train]
dim(train.X)
summary(Smarket$Direction)
summary(Smarket$Direction)

# Make predictions for dates in 2005

# k=1
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred,Direction.2005)

(83+43)/252

# k=3
knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

detach(Smarket) # Cleanup so we don't have bad references to wrong table

# QDA provides best results so far

## Caravan Insurance Data ####

dim(Caravan)

dim(Caravan)
#View(Caravan)
attach(Caravan)
summary(Caravan$Purchase)
348/5822 # 6% purchased Caravan insurance

# Salary and age are on different scales
summary(Caravan[,1])

# Now every column of standardized.X has a standard deviation of one and a mean of zero.
standardized.X=scale(Caravan [,-86]) # Produces matrix for all columns
summary(standardized.X[,1])

var(Caravan [ ,1])
var(Caravan [ ,2])

var(standardized.X[,1])
var(standardized.X[,2])
mean(standardized.X[,1])

#
test=1:1000

train.X=standardized.X[-test ,] # Exclude first 1000 rows

test.X=standardized.X[test ,]

train.Y=Caravan$Purchase [-test]
test.Y=Caravan$Purchase [test]

# Predict
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)

# Evaluate
mean(test.Y!=knn.pred) # 0.118  11.8%
mean(test.Y!="No") # 0.059 6%


table(knn.pred,test.Y) # Confusion Matrix
# Yes  68   9
9/(68+9) # Success rate = 11.7%

# K=3 ####
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y) # Confusion Matrix
# Yes  21   5
5/(21 + 5) # Success rate = 19.2%

# K=5 ####
knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y) # Confusion Matrix
# Yes  11   4
4/(11+4) # Success rate = 26%

# Fit a logistic regression model to the data

# 1. Fit
glm.fits=glm(Purchase~.,data=Caravan ,family=binomial, subset=-test)

# 2. Predict
glm.probs=predict(glm.fits,Caravan[test,], type="response")

# 3. Evaluate
glm.pred=rep("No",1000) # The 1000 in test
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,test.Y)
# Yes   7   0

glm.pred=rep("No",1000) # The 1000 in test
glm.pred[glm.probs >.25]="Yes"
table(glm.pred,test.Y)
#Yes  22  11
11/(22+11) # Success rate = 33.3%

#
detach(Caravan)  # Cleanup so we don't have bad references to wrong table

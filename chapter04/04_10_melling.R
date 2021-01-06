# Chapter 4, Question 10
# p171
library(ISLR)
library(tidyverse)

# 10a ####
summary(Weekly)

pairs(Weekly)

# 10b: Logistic Regression ####

names(Weekly)
glm.fit1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)  # binomial for logistic regression
summary(glm.fit1)    

# Answer
# Lag2, other predictors appear statistically significant: All Pr(>|z|) > 0.05


# 10c: Confusion Matrix ####

glm.probs = predict(glm.fit1, type = "response")
contrasts(Weekly$Direction)
summary(Weekly$Direction)

### Confusion Matrix ####
# Create prediction table
nrow(Weekly)
glm.pred = rep("Down", nrow(Weekly)) 
glm.pred[glm.probs > .5] = "Up"

glm.pred

summary(glm.pred)

# Produce a confusion matrix
# Diagonals indicate correct predictions. Off-diagonals indicate incorrect predictions.
table(glm.pred, Weekly$Direction) 

# glm.p Down  Up
# Down   54  48
# Up    430 557

# 430 False Up's
# 48
605/1809
430/(430 + 557)
430/1809
48/1809
(430 + 557)/1806
mean(glm.pred == Weekly$Direction) ## Correct 56%
mean(glm.pred != Weekly$Direction) ## Incorrect

# Saying Up 430 times when it shouldn't?

# 10d: ####
summary(Weekly$Year)
train = (Weekly$Year<2009)

test_data_weekly.2009 = Weekly[!train,]

Direction.2009 = Weekly$Direction[!train]  # The Y's, the Response

glm.fit1 = glm(Direction ~Lag2, data = Weekly, family = binomial, subset = train) # Train Data
glm.probs2 = predict(glm.fit1, test_data_weekly.2009, type = "response")

glm.pred = rep("Down", nrow(test_data_weekly.2009))
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2009)

mean(glm.pred == Direction.2009) # % Correct Predictions 
mean(glm.pred != Direction.2009) # Compute and test error rate

# 10e: LDA ####

## Linear Discriminant Analysis ####

library(MASS)

lda.fit1 = lda(Direction ~ Lag2,
               data = Weekly, 
               subset = train)  # train boolean vector

lda.fit1
summary(lda.fit1)    

## Need? test_data_weekly.2009

#lda.pred = predict(lda.fit1, test_data_weekly.2009, type = "response") # This one?
lda.pred = predict(lda.fit1, test_data_weekly.2009) # This one!! Default type="response"?

summary(lda.pred) # class, posterior, x - See Lab

lda.class = lda.pred$class
# specificity vs Sensitivity

table(lda.class, Direction.2009) # Confusion Matrix Predicted vs Truth
Direction.2009
mean(lda.class == Direction.2009) # Accurate 62% of the time

sum(lda.pred$posterior[,1] >=.5) # Using column Down, Out of 104 in test data 75 are
sum(lda.pred$posterior[,1] <.5)

#View(lda.pred$posterior)
head(lda.pred$posterior)
sum(lda.pred$posterior[,1] >.9) # Want only over 90% posterior probability

# 10f: QDA ####

## Step 1: Train the Model

qda.fit = qda(Direction ~ Lag2,
               data = Weekly, 
               subset = train)  # train boolean vector
qda.fit
summary(qda.fit)    

## Step 2: Use Trained model to predict Test Data
qda.pred = predict(qda.fit, test_data_weekly.2009)

## Step 3: Evaluate the fit of our Test Data

qda.class = qda.pred$class
table(qda.class, Direction.2009) # Confusion Matrix

mean(qda.class == Direction.2009) # Accurate 58% of the time


# 10g: KNN ####

## K-Nearest Neighbors ####

library(class)

# Lab Example doesn't work

# train.X = cbind(Lag2)[train,] # cbind??
# test.X = cbind(Lag2)[!train,]

train.X = as.matrix(Weekly$Lag2[train])
test.X = as.matrix(Weekly$Lag2[!train])

train.Direction = Weekly$Direction[train]

summary(Weekly$Direction[train])

summary(train.Direction)
length(train.Direction)
length(train.X)
length(test.X)
set.seed(1)
dim(train.X)
dim(test.X)

knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2009)
Direction.2009
mean(knn.pred == Direction.2009) # 50% correct


# 10h: Which of these methods appears to provide the best results on this data? ####

# LDA accurate 62% of the time

# 10i: ####



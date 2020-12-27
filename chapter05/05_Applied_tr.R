#### Applied #5 ####

# Setting the seed
set.seed(1)

# Importing libraries
library(ISLR)
names(Default)
dim(Default) # (10000, 4)

#### 5(a) ####

# Fitting logistic regression on
lr.fit = glm(default ~ income + balance, data=Default, family=binomial)
summary(lr.fit)

#### 5(b) ####

#### 5(b)i ####

# Getting indicies of train data 
train = sample(nrow(Default), nrow(Default)/2)

#### 5(b)ii ####

# Fitting train data using LR
lr.train.fit = glm(default ~ income + balance, data=Default, family=binomial, subset=train)
summary(lr.train.fit)

#### 5(b)iii ####

# Probabilities for entire dataset
lr.prob = predict(lr.train.fit, Default)

# Probabilities for validation set
lr.prob.val = lr.prob[-train]

# Predictions
lr.pred = rep("No", length(lr.prob.val))
lr.pred[lr.prob.val > 0.5] = "Yes"

#### 5(b)iv ####

# Calculating the validation set error rate
mean((Default$default[-train] != lr.pred))

#### 5(c) ####

train.model = function(train.split) {
  
  t.split = sample(nrow(Default), nrow(Default)*train.split)
  
  # Fitting train data using LR
  lr = glm(default ~ income + balance, data=Default, family=binomial, subset=t.split)
  
  # Probabilities for entire dataset
  lr.p = predict(lr, Default)
  
  # Probabilities for validation set
  lr.p.val = lr.p[-t.split]
  
  # Predictions
  pred = rep("No", length(lr.p.val))
  pred[lr.p.val > 0.5] = "Yes"
  
  # Calculating the validation set error rate
  return(mean((Default$default[-t.split] != pred)))

}

train.model(.5)
train.model(.7)
train.model(.8)
train.model(.9)
train.model(.3)

#### 5(d) ####

# Fitting model using Student
lr.student = glm(default ~ ., data=Default, family=binomial, subset=train)

# Probabilities for entire dataset
lr.student.prob = predict(lr.student, Default)

# Probabilities for validation set
lr.student.prob.val = lr.student.prob[-train]

# Predictions
lr.student.pred = rep("No", length(lr.student.prob.val))
lr.student.pred[lr.student.prob.val > 0.5] = "Yes"

# Calculating the estimated validation set error rate
mean((Default$default[-train] != lr.student.pred))

## Comments: Addin the student predictor does not seem to decrease the error rate.
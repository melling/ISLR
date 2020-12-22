# Chapter 4, Question 10

library(ISLR)
library(tidyverse)

View(Weekly)

w2 = tibble(Weekly)
summary(w2)

# 10a ####
summary(Weekly)
attach(Weekly)
pairs(Weekly)

# 10b: Logistic Regression ####

glm.fit1 = glm(Direction ~ ., data = Weekly, family = binomial)  # binomial for logistic regression
summary(glm.fit1)    

# No predictors appear statistically significant: All Pr(>|z|) > 0.05


# 10c: Confusion Matrix ####

glm.probs = predict(glm.fit1, type = "response")
View(glm.probs)
contrasts(Direction)

### Confusion Matrix ####
# Create prediction table
nrow(Weekly)
glm.pred = rep("Down", nrow(Weekly)) 
glm.pred[glm.probs > .5] = "Up"
nrow(glm.pred)
glm.pred
typeof(glm.pred)
summary(glm.pred)
# Produce a confusion matrix
# Diagonals indicate correct predictions. Off-diagonals indicate incorrect predictions.
table(glm.pred, Direction) 

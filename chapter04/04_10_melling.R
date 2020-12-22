# Chapter 4, Question 10

library(ISLR)
library(tidyverse)

#View(Weekly)

# w2 = tibble(Weekly)
# summary(w2)

# 10a ####
summary(Weekly)
# attach(Weekly)
pairs(Weekly)

# 10b: Logistic Regression ####

names(Weekly)
glm.fit1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)  # binomial for logistic regression
summary(glm.fit1)    

# Answer
# Lag2, other predictors appear statistically significant: All Pr(>|z|) > 0.05


# 10c: Confusion Matrix ####

glm.probs = predict(glm.fit1, type = "response")
#View(glm.probs)
contrasts(Weekly$Direction)

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
mean(glm.pred == Weekly$Direction) ## Correct
mean(glm.pred != Weekly$Direction) ## Incorrect

# Saying Up 430 times when it shouldn't?

# 10d: ####
summary(Weekly$Year)
train = (Weekly$Year<2009)
weekly.2009 = Weekly[!train,]

Direction.2009 = Weekly$Direction[!train]

glm.fit1 = glm(Direction ~Lag2, data = Weekly, family = binomial, subset = train) # Train Data
glm.probs2 = predict(glm.fit1, weekly.2009, type = "response")

glm.pred = rep("Down", nrow(weekly.2009))
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2009)

mean(glm.pred == Direction.2009) # % Correct Predictions 
mean(glm.pred != Direction.2009) # Compute and test error rate

# 10e: ####

# 10f: ####


# 10g: ####

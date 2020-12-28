# Chapter 5, Q7

library(ISLR)

set.seed(1)

# 7a ####

lr.fit = glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)
summary(lr.fit)


# 7b ####

train = Weekly[-1,]
test = Weekly[1,]

lr.fit.b = glm(Direction ~ Lag1 + Lag2, data=train, family=binomial)
summary(lr.fit.b)


# 7c: Predict ####

lr.prob = predict(lr.fit.b, test, type = "response") # 0.287534

lr.prob
lr.prob > 0.5 # >0.05 means LR predicts up
Weekly[1,]$Direction # Actual direction was down Down

# Answer: Predicted incorrectly that it went Up

# 

# 7d: Predict ####

num_incorrect = 0

for (i in 1:nrow(Weekly)) {
  train = Weekly[-i,]
  test = Weekly[i,]
  lr.fit.b = glm(Direction ~ Lag1 + Lag2, data=train, family=binomial)
  lr.prob = predict(lr.fit.b, test, type = "response")
  
  if (lr.prob > 0.5) {
    predicted_direction = "Up"
  } else {
    predicted_direction = "Down"
  }
  #is_not_correct = Weekly[i,"Direction"] != predicted_direction
  if (Weekly[i,"Direction"]  != predicted_direction) {
    num_incorrect = num_incorrect + 1
  }
  
  
}
print(num_incorrect)

# 7e: Predict ####
num_incorrect
num_incorrect/nrow(Weekly)  # 45% wrong

#LOOCV


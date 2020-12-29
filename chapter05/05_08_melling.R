# Chapter 5, Q8

library(ISLR)

# 8a ####

set.seed(1)
x=rnorm(100)
y=x - 2*x^2 + rnorm(100)
# n = 100
# p = 2

# Y = b0 + b1x1 + ...+ bpXp 

# 8b ####

plot(x,y)

# Non-linear relationship.  Quadratic

# 8c ####

library(boot) # cv.glm()

# More on poly()
# https://stackoverflow.com/questions/19484053/what-does-the-r-function-poly-really-do




loocv.fn = function(df, exponent) {
  glm.fit = glm(y~poly(x, exponent), df)
}


set.seed(1)
df = data.frame(x,y)

cv.error=rep(0,5) # 5 zeros
#(0,0,0,0,0)

for (i in 1:5) {
  print(i)
  glm.fit=glm(y~poly(x, i))
  print(cv.glm(df, glm.fit)$delta)
#  cv.error[i] = cv.glm(df, glm.fit)$delta[]
}

#cv.error


# 8d ####
set.seed(10)
cv.error=rep(0,5) # 5 zeros

for (i in 1:5) {
  print(i)
  glm.fit=glm(y~poly(x, i))
  #print(cv.glm(df, glm.fit)$delta)
  cv.error[i] = cv.glm(df, glm.fit)$delta[1]
}

plot(1:5, cv.error)

# We get the same answer because LOOCV only leaves out one data point out
# Cycles through all points one at a time to train.

# 8e ####

# Because original equation was quadratic, the quadratic fit will match the best

# 8f ####
summary(glm.fit)

# The linear and quadratic have significance p-value < 0.05


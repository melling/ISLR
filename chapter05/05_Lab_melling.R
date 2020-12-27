# Chapter 5 Lab

# 5.3.1 Validation Set Approach ####

library(ISLR)

attach(Auto)

set.seed(1)

train = sample(392,196) # 1..392 - Take 196 i.e. Half

lm.fit = lm(mpg~horsepower, data = Auto, subset = train)

## ###
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2) # 25.569

# ^2
lm.fit2=lm(Auto$mpg~poly(Auto$horsepower ,2),data=Auto,subset=train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2) # 20.9

# ^3
lm.fit3=lm(Auto$mpg~poly(Auto$horsepower ,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) # 20.87186

# Choose different training set
# Get different means

set.seed (2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower ,subset=train)

mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2) #

# ^2
lm.fit2=lm(Auto$mpg~poly(Auto$horsepower ,2),data=Auto,subset=train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2) # Still best

# ^3
lm.fit3=lm(Auto$mpg~poly(Auto$horsepower ,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) # 

# 5.3.2 Leave-One-Out Cross-Validation ####

glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit) # Same as lm.fit

lm.fit=lm(mpg~horsepower ,data=Auto)
coef(lm.fit)

library(boot)


glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)
cv.err=cv.glm(Auto,glm.fit)

# correspond to the LOOCV statistic given in (5.1)
cv.err$delta # cross-validation results


cv.error=rep(0,5) # 5 zeros
for (i in 1:5) {
  print(i)
  glm.fit=glm(mpg~poly(horsepower, i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}

cv.error

set.seed(17)
cv.error=rep(0,10) # 10 zeros
for (i in 1:10) {
  print(i)
  glm.fit=glm(mpg~poly(horsepower, i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit, K = 10)$delta[1]
}
cv.error

# 5.3.4 The Bootstrap ####

# Formula (5.7)

alpha.fn=function(data,index) {
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}

#View(Portfolio)

# Portfolio data
alpha.fn(Portfolio, 1:100) # 0.5758321


set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) # 0.7368375 WRONG!
# 0.596

boot(Portfolio, alpha.fn, R=1000)

# Estimating the Accuracy of a Linear Regression Model ####

boot.fn=function(data,index)
  return(coef(lm(mpg∼horsepower ,data=data,subset=index))) > boot.fn(Auto ,1:392)


detach(Auto)

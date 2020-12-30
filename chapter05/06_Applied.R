#### 6 ####

# Importing libraries
library(ISLR)
library(boot)

# Viewing some info of Default
names(Default)
dim(Default)
summary(Default)

#### 6(a) ####

# Fitting Logistic Regression on default using income and balance
glm.fit = glm(default ~ income + balance, data=Default, family=binomial)

# Viewing stats on logistic regression fit
summary(glm.fit)

## SE 4.348e-1, 4.985e.6, 2.274e-4 for coefficients of intercept, income, balance, 
## respectively


#### 6(b) ####

# Function coefficiencts of a model
boot.fn = function(data, index) {
  return(coef(glm(default ~ income + balance, data=data, subset=index, family=binomial)))
}


#### 6(c) ####

# Applying boostrap on boot.fn
boot(Default, boot.fn, 1000)

### 6(d) ####

## pg.196
## Comments: The estimates in glm and bootstrap are slightly different. However, the 
## bootstrap estimates are more accurate than glm because it does not make any assumptions
## that linear regressions makse such as data has a linear relationship.

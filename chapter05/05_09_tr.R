#### 9 ####

# Importing libraries
library(MASS)
library(boot)

names(Boston)
summary(Boston)
dim(Boston)

#### 9(a) ####

# Calculating the mean medv for sample 
mean.hat = mean(Boston$medv)
print(mean.hat) # 22.5328

#### 9(b) #####

# Calculating the standard error of medv
sd(Boston$medv)/sqrt(nrow(Boston)) # 0.4089

#### 9(c) ####

# Bootstrap 
boot.fn = function(data, index) {
  return(mean(data[index]))
}

# Using bootstrap to figure out an estimate for population mean
bstrap = boot(Boston$medv, boot.fn, 1000)
print(bstrap) # 0.3994119

## Comments: The SE of bootstrap is  different from (b) by about 0.001. 

#### 9(d) ####

# Creating a 95% confidence interval 
conf.int = c(bstrap$t0 - (2 * 0.3994119), bstrap$t0 + (2  * 0.3994119))

# 
t.test(Boston$medv)

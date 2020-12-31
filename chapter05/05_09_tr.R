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

#### 9(e) ####

median_hat = median(Boston$medv)
print(median_hat) # 21.2

#### 9(f) ####

# Calculating median of data
boot.median.fn = function(data, index) {
  return(median(data[index]))
}

# Running bootstrap for median
bstrap.median = boot(Boston$medv, boot.median.fn, 10000)
print(bstrap.median) # 0.3754

## Comments: Median is the same, whether applying boostrap or just median(). The SE, 0.38,
## is small compared to the median value of 21.2.

#### 9(g) ####

quant.ten = quantile(Boston$medv, probs=c(.1)) # 12.75 

#### 9(h) ####

# Calculating 10%  quantile  of data
boot.quant.ten.fn = function(data, index) {
  return(quantile(data[index], probs=c(.1)))
}

# Running bootstrap for median
boot.quant.ten = boot(Boston$medv, boot.quant.ten.fn, 10000)
print(boot.quant.ten) # SE: 0.5027

## Comments: There was no difference with the 10% quartile, There was a SE of .50.

#### 08 Applied ####

library(ISLR)

#### 8(a) ####

set.seed(1)

num.rows = nrow(Carseats)

# Sample of indices for training
train = sample(1:num.rows, num.rows*0.7)

# Test set
x.train = Carseats[train,]
y.train = Carseats$Sales[train]
x.test = Carseats[-train,]
y.test = Carseats$Sales[-train]


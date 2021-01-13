#### 10 ####

library(leaps)

#### 10(a) ####

set.seed(1)

# Randomly generating X (1000 observations with 20 predictors)

p = 20
n = 1000

X <- matrix(ncol=p, nrow=n)

for(i in 1:n){
  X[i,] <- rnorm(p)
}

# Randomly generating Beta values with few being 0's
B = rnorm(p)

for (i in c(1, 5, 8, 14, 17)) {
  B[i] = 0
}
  
# Randomly generating error terms
e = rnorm(1000)

# Creating the model

Y = X%*%B + e

# Creating a dataframe of X predictors and y response
df = data.frame(X, Y)

#### 10(b) ####

train = sample(x=nrow(df), size=100, replace=FALSE)
train_df = df[train,]
test_df = df[-train,]

#### 10(c) ####

# Fitting 
bss.fit = regsubsets(Y ~ ., data=train_df, nvmax=p)
summary(bss.fit)

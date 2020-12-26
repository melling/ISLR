#### 13 ####

# Importing libraries
library(ISLR)
library(MASS)

# Viewing the column names
names(Boston)
dim(Boston)

#### Data Preparation ####

# Creating a class variable: 1 for above crim median and 0 otherwise
# Creating a vector that returns 1 if >= median(Auto$mpg) else 0
crim01 = rep(0, nrow(Boston))
crim01[Boston$crim >= median(Boston$crim)] = 1

# Creating a new dataframe combining Boston and crim01
new.Boston = data.frame(Boston, crim01)

# Viewing the pairs
pairs(new.Boston)

## Comments: Not that many varibles are linearly related to the crim01 variable.
## My guess would be that the QDA and KNN will produce better results than LDA

# Setting seed
set.seed(1)

# Creating sample size for train data (80% of the data)
sample.size = floor(0.7*nrow(new.Boston))

# Get random sample of indices for train data
train = sample(seq_len(nrow(new.Boston)), size=sample.size)

# Creating a train set
X.train = new.Boston[train,]
y.train = new.Boston$crim01[train]

# Creating a test set
X.test = new.Boston[-train,]
y.test = X.test$crim01

#### Logistic Regression ####

# Fitting LR data on traininig data
glm.fit = glm(crim01 ~ zn + nox + dis + age, data=new.Boston, subset=train,
              family=binomial)

# Predicting the probabilities of test values using the fitted LR
glm.probs = predict(glm.fit, X.test, type="response")

# Creating a predctions vector and filling in 1 when probability is >0.5, else 0
glm.pred = rep(0, nrow(X.test))
glm.pred[glm.probs > .5] = 1

# Generating a confusion matrix using table()
table(glm.pred, y.test)

# Calulating the test-error rate
mean(glm.pred != y.test) # 19.1%

#### Linear Discriminant Analysis ####

# Fitting a LDA model on data
lda.fit = lda(crim01 ~ zn + nox + dis + age, data=new.Boston, subset=train)

# Predicting mpg01 using the test data
lda.pred = predict(lda.fit, X.test)

# Generating a confusion matrix
lda.class = lda.pred$class
table(lda.class, y.test) 

# Calulating the test-error rate
mean(lda.class != y.test) # 12.5%

#### Quadratic Discriminant Analysis ####

# Fitting a QDA  model on data
qda.fit = qda(crim01 ~ zn + nox + dis + age, data=new.Boston, subset=train)

# Predicting mpg01 using the test data
qda.pred = predict(qda.fit, X.test)

# Generating a confusion matrix
qda.class = qda.pred$class
table(qda.class, y.test) 

# Calulating the test-error rate
mean(qda.class != y.test) # 15.7%

#### K-Nearest Neighbors ####

# Importing libraries
library(class)

# Binding predictors of new.Boston data for training set
train.X = cbind(new.Boston$zn, new.Boston$nox, new.Boston$dis, new.Boston$age)[train,]

# Creating response for training set
train.crim = new.Boston$crim01[train]

# Binding predictors of new.Boston datafor test set
test.X = cbind(new.Boston$zn, new.Boston$nox, new.Boston$dis, new.Boston$age)[-train,]

# Fitting data using KNN model
knn.pred = knn(train.X, test.X, train.crim, k=1)

# Creating a confusion matrix
table(knn.pred, y.test)

mean(knn.pred != y.test) # 26.3%

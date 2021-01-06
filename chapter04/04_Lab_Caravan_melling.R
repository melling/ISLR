## Caravan Insurance Data ####
# p165

dim(Caravan)
#View(Caravan)
attach(Caravan)
summary(Caravan$Purchase)
348/5822 # 6% purchased Caravan insurance

# Salary and age are on different scales
summary(Caravan[,1])

# Now every column of standardized.X has a standard deviation of one and a mean of zero.
standardized.X=scale(Caravan [,-86]) # Produces matrix for all columns
summary(standardized.X[,1])

var(Caravan [ ,1])
var(Caravan [ ,2])

var(standardized.X[,1])
var(standardized.X[,2])
mean(standardized.X[,1])

#
test=1:1000

train.X=standardized.X[-test ,] # Exclude first 1000 rows

test.X=standardized.X[test ,]

train.Y=Caravan$Purchase [-test]
test.Y=Caravan$Purchase [test]

# Predict
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)

# Evaluate
mean(test.Y!=knn.pred) # 0.118  11.8%
mean(test.Y!="No") # 0.059 6%


table(knn.pred,test.Y) # Confusion Matrix
# Yes  68   9
9/(68+9) # Success rate = 11.7%

# K=3 ####
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y) # Confusion Matrix
# Yes  21   5
5/(21 + 5) # Success rate = 19.2%

# K=5 ####
knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y) # Confusion Matrix
# Yes  11   4
4/(11+4) # Success rate = 26%

# Fit a logistic regression model to the data

# 1. Fit
glm.fits=glm(Purchase~.,data=Caravan ,family=binomial, subset=-test)

# 2. Predict
glm.probs=predict(glm.fits,Caravan[test,], type="response")

# 3. Evaluate
glm.pred=rep("No",1000) # The 1000 in test
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,test.Y)
# Yes   7   0

glm.pred=rep("No",1000) # The 1000 in test
glm.pred[glm.probs >.25]="Yes"
table(glm.pred,test.Y)
#Yes  22  11
11/(22+11) # Success rate = 33.3%

#
detach(Caravan)  # Cleanup so we don't have bad references to wrong table

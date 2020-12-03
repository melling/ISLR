library(ISLR)

### Stock Market Data ####

names(Smarket)
dim(Smarket)

summary(Smarket)

cor(Smarket[,-9]) # Skip col 9 because it's qualitative

attach(Smarket)
plot(Volume)

### Logistic Regression ####

glm.fit1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)  # binomial for logistic regression
summary(glm.fit1)              

coef(glm.fit1)

summary(glm.fit1)$coef
summary(glm.fit1)$coef[,4] # Column 4

# P(Y=1|X)
glm.probs = predict(glm.fit1, type = "response")
glm.probs[1:10]

contrasts(Direction)

### Confusion Matrix ####
# Create prediction table

glm.pred = rep("Down", 1250) 
glm.pred[glm.probs > .5] = "Up"

# Produce a confusion matrix
# Diagonals indicate correct predictions. Off-diagonals indicate incorrect predictions.
table(glm.pred, Direction) 

# Determine how many correctly and incorrectly classified
(507 + 145) / 1250 # Correct 52.16%

mean(glm.pred == Direction)
glm.pred

train = (Year<2005)
train[1:20]
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
Smarket.2005[1:10,]
head(Smarket.2005)

### Fit Logistic Regression Model ####

glm.fit2 = glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs2 = predict(glm.fit2, Smarket.2005, type = "response")

# Now check prediction

glm.pred = rep("Down", 252)
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) # Compute and test error rate

### Remove Predictors ####

# remove +Lag3+Lag4+Lag5+Volume
glm.fit1 = glm(Direction ~Lag1+Lag2, data = Smarket, family = binomial, subset = train) # Train Data
glm.probs2 = predict(glm.fit1, Smarket.2005, type = "response")

# Again check

glm.pred = rep("Down", 252)
glm.pred[glm.probs2 > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
106 / (106 + 76)

predict(glm.fit1, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type = "response")

## Linear Discriminant Analysis ####


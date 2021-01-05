# ISLR Chapter 4 Lab Modified to solve Kaggle Titanic Project
# GLM Solution
# Kaggle Score: 0.76555

titanic=read.csv("input/titanic_train.csv")
titanic_test=read.csv("input/titanic_test.csv")

### Logistic Regression ####

glm.fit1 = glm(Survived ~ Sex + Pclass, data = titanic, family = binomial)  # binomial for logistic regression
summary(glm.fit1)              

coef(glm.fit1)

summary(glm.fit1)$coef
summary(glm.fit1)$coef[,4] # Column 4

glm.probs = predict(glm.fit1, type = "response") # Uses training data by default
#glm.probs[1:10]

#Titanic_Validation = titanic[!train,]

glm.pred = rep("No", nrow(titanic))
glm.pred[glm.probs > .5] = "Yes"
summary(glm.pred)

table(glm.pred, titanic$Survived)
(468+233)/nrow(titanic) # Correct 0.7867565

head(glm.pred)
head(titanic$Survived)

mean(glm.pred == titanic$Survived) # Use 0/1 instead of Yes/No


## Now predict the training set

glm.probs_test = predict(glm.fit1, titanic_test, type = "response")
titanic_test$Survived = 0

glm.pred_test = rep(0, nrow(titanic_test))
glm.pred_test[glm.probs_test > .5] = 1
summary(glm.probs_test)
#length(glm.pred_test)
head(glm.pred_test)
#table(glm.pred_test, titanic_test)
#contrasts(titanic$Survived)

#glm.probs = predict(glm.fit1, newdata = titanic_test, type = "response")

length(glm.probs)

out <- data.frame(
  PassengerId=titanic_test$PassengerId,
  Survived=glm.pred_test,
  row.names=NULL)

## Output File ####
getwd()
print("Writing output")
write.csv(x=out,
          file='islr_ch4_glm.csv',
          row.names=FALSE,
          quote=FALSE)
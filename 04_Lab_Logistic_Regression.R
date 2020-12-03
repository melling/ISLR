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


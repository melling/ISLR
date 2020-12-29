# Chapter 5, Q6

library(ISLR)

# 6a ####

lr.fit = glm(default ~ income + balance, data=Default, family=binomial)
summary(lr.fit)


# 6b: boot ####

# Chapter 6, Q11
# p264

#library(ISLR)
library(MASS)
library(leaps)

n = nrow(Boston)
dim(Boston)[2]-1
train = sample(1:n, n*.7)
#test = (-train)
bss.fit <- regsubsets(crim ~ ., data = Boston[train,], method = "exhaustive", nvmax = dim(Boston)[2]-1)
summary.fit = summary(bss.fit)

plot(summary.fit$bic) # lower is better
bic.min = which.min(summary.fit$bic)
bic.min
points(bic.min, summary.fit$bic[bic.min], col=2, cex=2, pch=20)

plot(bss.fit)

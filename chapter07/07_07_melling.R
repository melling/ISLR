# Chapter 7 Question 7
# p299

library(tidyverse)
library(ISLR)

attach(Wage)
# View(Wage)
pairs(maritl, jobclass)
pairs(Wage)
plot(jobclass,wage)
plot(maritl,wage)

# jobclass.fit = lm(wage~I(jobclass), data = Wage)
ggplot(Wage, aes(x = maritl, y = wage, fill = maritl)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

jobclass.fit = lm(wage~I(jobclass), data = Wage)
#anova(fit1,fit2,fit3)
#cut(Wage$jobclass, 4)

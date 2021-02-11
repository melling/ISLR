#getwd()
#setwd("../chapter02/")
#setwd("chapter02/")
# 8.a
college = read.csv("../input/College.csv", stringsAsFactors = TRUE)

# 8.b ####
# fix(college) 
rownames(college) = college[,1]

college = college[,-1]

# 8.c.i  ####
summary(college)

# 8.c.ii####
pairs(college[,1:10])

# 8.c.iii ####
plot(college$Outstate, college$Private)
plot(college$Private, college$Outstate)

# 8.c.iv ####
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

summary(college$Elite)
plot(college$Elite, college$Outstate)

rownames(college[college$Elite == "Yes",])

# 8.c.v ####
attach(college)
par(mfrow=c(2,2))
hist(Enroll, breaks = 100)
hist(Accept, breaks = 100)
hist(PhD, breaks = 100)
hist(Books, breaks = 100)

summary(PhD)
summary(Enroll)

library(ISLR)
rownames(college[college$Enroll >= 5000,])

# 9.a ####
colnames(Auto)
quantitative =  c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")
qualitative = c("origin", "name")  

# 9.b ####
attach(Auto)
for (col in quantitative)
{
  print(col)
  print(range(Auto[col]))
}

sd(Auto$mpg)

# 9.c ####
# for (col in quantitative){
#   print(col)
#   print(mean(Auto[col], na.rm=TRUE))
#   # print(sd(Auto[col]))
# }
sd(Auto$mpg)
sd(Auto$cylinders)
sd(Auto$displacement)
sd(Auto$weight)
sd(Auto$horsepower)
sd(Auto$acceleration)
sd(Auto$year)

mean(Auto$mpg)
mean(Auto$cylinders)
mean(Auto$displacement)
mean(Auto$weight)
mean(Auto$horsepower)
mean(Auto$acceleration)
mean(Auto$year)

# 9.d ####
auto = auto[-c(10:85),]
attach(auto)
sd(mpg)
sd(cylinders)
sd(displacement)
sd(weight)
sd(horsepower)
sd(acceleration)
sd(year)

mean(mpg)
mean(cylinders)
mean(displacement)
mean(weight)
mean(horsepower)
mean(acceleration)
mean(year)

# 9.e ####

auto = Auto
pairs(auto)
plot(acceleration, weight)
plot(mpg, weight)
plot(weight, mpg)
plot(origin, mpg)
plot(origin, acceleration)

# 9.f ####
# the variables we will use are the ones that are highly correlated with 
# mpg such as weight, displacement, year, horsepower

# 10.a ####
library(MASS)
?Boston

dim(Boston)
# rows: 506
# columns: 14

# 10.b ####
pairs(Boston)
pairs(~ crim + age, Boston)
pairs(~ crim + age + zn, Boston)
pairs(~ crim + age + nox + medv , Boston)

# 10.c ####
# Age of the housing, Most likely the older the neighborhood, the more experience/ lower income it has with crime
# Radial Highways, either criminals has quick getaway or it is so far away from highway that it is not gentrified
# 

# 10.d ####
Boston[Boston$crim >= 20,]
Boston[Boston$tax >= 666,]
Boston[Boston$ptratio >= 20,]

# 10.e ####
dim(Boston[Boston$chas==1,])[1]

# 10.f ####
median(Boston$ptratio)

# 10.g ####
Boston[Boston$medv == min(Boston$medv),]

# 10.h ####
dim(Boston[Boston$rm > 7,])[1]
dim(Boston[Boston$rm > 8,])[1]
pairs(Boston[Boston$rm > 8,])

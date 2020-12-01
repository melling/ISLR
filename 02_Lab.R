# http://faculty.marshall.usc.edu/gareth-james/ISL/bios.html

## Basic Commands ####
x <- c(1,3,2,5)
x

x = c(1,6,2)
y = c(1,4,3)
length(x)
length(y)
ls() # List all objects
rm(list = ls()) # Remove all objects

x = matrix(data=c(1,2,3,4), nrow = 2, ncol = 2)
x

x = matrix(c(1,2,3,4), 2, 2)
x

matrix(c(1,2,3,4), 2, 2, byrow = TRUE)

sqrt(x)

x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
y
cor(x,y)

set.seed(1303) # Produce same random results by initializing the seed.
rnorm(50) # First 50
rnorm(50) # Next 50

set.seed(3) # Matches book
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y)) # sd()
sd(y)

## Graphics ####

x=rnorm(100)
y=rnorm(100)

plot(x,y, xlab = "x-axis", ylab = "y-axis", main = "Title")

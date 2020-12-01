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

### Save chart as PDF ####

pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

### Sequences ####

x = seq(1,10)
x
x = 1:10
x
x = seq(-pi,pi, length = 50)
x

### Contour Plot ####

y=x
f=outer(x,y,function(x,y) cos(y) / (1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels = 45, add = T)

fa = (f-t(f))/2
contour(x,y,fa,nlevels = 15)

### Image() - heatmap ####
image(x,y,fa)

### Perspective Plots ####

persp(x,y,fa)
persp(x,y,fa,theta = 30)
persp(x,y,fa,theta = 30, phi = 20)
persp(x,y,fa,theta = 30, phi = 70)
persp(x,y,fa,theta = 30, phi = 40)

## Indexing Data ####

A = matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)] # rows 1 and 3 intersect with columns 2 and 4
A[c(1:3),c(2:4)] #

A[1:2,]
A[1,]

### Negative sign excludes

A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]

dim(A)

## Loading Data ####

Auto = read.table("Auto.data")
#  fix(Auto) # X not working on Mac

Auto = read.table("Auto.data", header = T, na.strings = "?")

Auto = read.csv("Auto.csv", header = T, na.strings = "?")

#### 9 ####

library(ISLR)
library(MASS)
library(gam)

#### 9(a) ####

# Fitting a poly 
poly.nox = lm(nox ~ poly(dis,3), data=Boston)

# Viewing the summary
summary(poly.nox)

# Plotting the fit
par(mfrow=c(1,1))
plot(Boston$dis, Boston$nox)
plot(poly.nox, se=TRUE, col='blue')


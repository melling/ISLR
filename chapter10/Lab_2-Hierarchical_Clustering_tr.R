#### 10.5.2 Hierarchical Clustering ####

# Setting the seed
set.seed(2)

# Creating a matrix of two columns with 50 values each
x = matrix(rnorm(50*2), ncol=2)

# Creating separation between data

## Adding three to the first 25 values of the first column
x[1:25,1] = x[1:25,1] + 3

## Subtracting four from the first 25 values of the second column
x[1:25,2] = x[1:25,2] - 4

## Plotting
plot(x[,1], x[,2])

## Running hierarchical Clustering
hc.complete = hclust(
  dist(x), # dist() computes inter-observation Euclidean distance matrix
  method="complete"
)

summary(hc.complete)

# Using average method
hc.average = hclust(dist(x), method="average")

# Using single method
hc.single = hclust(dist(x), method="single")

# Plotting dendrograms
par(mfrow=c(1,3))
plot(
  hc.complete,
  main="Complete Linkage",
  xlab="",
  sub="",
  cex=.9
)

plot(
  hc.average,
  main="Average Linkage",
  xlab="",
  sub="",
  cex=.9
)

plot(
  hc.single,
  main="Single Linkage",
  xlab="",
  sub="",
  cex=.9
)

# Viewing the cluster labels for each observation
cutree(hc.complete, 2)

# Viewing the cluster labels for each observation
cutree(hc.average, 2)

# Viewing the cluster labels for each observation
cutree(hc.single, 2)

# Scale x
xscale = scale(x)

par(mfrow=c(1,1))
plot(
  hclust(
    dist(xscale),
    method="complete"
  ),
  main="Hierarchical Clustering with Scaled Features"
)

# Correlation-based Distance

x = matrix(rnorm(30*3), ncol=3)
dd = as.dist(1-cor(t(x)))
plot(
  hclust(dd, method="complete"),
  main="Complete Linkage with Correlation-Based Distance",
  xlab="",
  sub=""
)

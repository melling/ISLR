#### Lab 2: Clustering ####

#### 10.5.1 K-Means Clustering ####

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

# Fitting K-means

km.out = kmeans(
  x=x, 
  centers=2, # number of clusters
  nstart=20 # number of random sets
)

# Viewing the cluster assignments
km.out$cluster
' COMMENT:
Determines the class by index
'

# Viewing the result in a table
table(km.out$cluster)

# Plotting clusters
plot(
  x, 
  col=(km.out$cluster+2), 
  main="K-Means Clustering Results with K=2",
  xlab="",
  ylab="",
  pch=20,
  cex=2
)

# Setting K=3
set.seed(4)
km.out = kmeans(x, 3, nstart=20)

# Viewing the summary of K-means
km.out

# Plotting clusters with 3
plot(
  x, 
  col=(km.out$cluster+2), 
  main="K-Means Clustering Results with K=3",
  xlab="",
  ylab="",
  pch=20,
  cex=2
)

# Comparing nstart=1 and nstart=20

## nstart=1
set.seed(3)
km.out = kmeans(x,3,nstart=1)
km.out$tot.withinss # total within-cluster sum of squares

## nstart=20
km.out = kmeans(x,3,nstart=20)
km.out$tot.withinss # total within-cluster sum of squares

## Viewing the individual within-cluster SS
km.out$withinss

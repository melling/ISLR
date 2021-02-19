#### Lab 3: NCI60 ####

# Using Clustering Algorithms on the NCI60 cancer cell microarray data

# Imports
library(ISLR)

# Getting the data 
nci.labels = NCI60$labs
nci.observations = NCI60$data

# Viewing the dimensions
dim(nci.observations)

# Viewing the cancer types
table(nci.labels)

#### 10.6.1 PCA on the NCI60 ####

# Fitting PCA
pr.out = prcomp(
  nci.observations, 
  scale=TRUE
)

# Assign a color to element in a vector
Cols = function(vec) {
 cols = rainbow(length(unique(vec))) 
 return(cols[as.numeric(as.factor(vec))])
}

# Plotting 
par(mfrow=c(1,2))

## Plotting Z1 and Z2 -> loading vector of PC
plot(
  pr.out$x[,1:2],
  col=Cols(nci.labels),
  pch=19,
  xlab="Z1",
  ylab="Z2"
)

## Plotting Z1 and Z3 -> loading vector of PC
plot(
  pr.out$x[,c(1,3)],
  col=Cols(nci.labels),
  pch=19,
  xlab="Z1",
  ylab="Z3"
)

# Viewing summary
summary(pr.out)

# Plotting the variance
plot(pr.out)

# Calculating the PVE
pve = 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(
  pve,
  type='o',
  ylab="Proportion of Variance Explained",
  xlab="Principcal Component",
  col="blue"
)

plot(
  cumsum(pve),
  type="o",
  ylab="Cumulative Proportion of Variance Explained",
  xlab="Principcal Component",
  col="brown3"
)

# Getting PVE from summary
summary(pr.out)$importance[2,]

# Getting cumulative sum of PVE from summary
summary(pr.out)$importance[3,]

#### 10.6.2 Clustering the Observations of the NCI60 Data ####

# Standardizing the variables
sd.data = scale(nci.observations)

# Plotting 
par(mfrow=c(3,1))
data.dist = dist(sd.data) 

# Plotting Dendrogram
plot(
  hclust(data.dist),
  labels=nci.labels,
  main="Complete Linkage",
  xlab="",
  ylab="",
  sub=""
)

plot(
  hclust(data.dist, method="average"),
  labels=nci.labels,
  main="Average Linkage",
  xlab="",
  ylab="",
  sub=""
)

plot(
  hclust(data.dist, method="single"),
  labels=nci.labels,
  main="Single Linkage",
  xlab="",
  ylab="",
  sub=""
)

# Using complete linkage
hc.out = hclust(
  dist(sd.data)
)

# Cutting dendrogram at height that will yield 4 clusters
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labels)

# Plotting dendrogram with abline cutting off at 4 clusters
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labels)
abline(h=139, col="red")

# Viewing hclust
hc.out

# Fitting K-means
set.seed(2)
km.out = kmeans(
  sd.data, 
  4, 
  nstart=20
)

# Extracting the K-Means clusters
km.clusters = km.out$cluster

# Comparing K-Means and Hierarchial Clustering
table(km.clusters, hc.clusters)

# Fitting hierachical clustering on PC
hc.out = hclust(
  dist(pr.out$x[,1:5])
)

# Plotting
plot(
  hc.out,
  labels=nci.labels,
  main="Hier. Clust. on First Five Score Vectors"
)

# Viewing accuracy
table(cutree(hc.out,4), nci.labels)

#### 9 ####  
'
Consider the USArrests data. We will now perform hierarchical clus- tering on the states.
(a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.
(b) Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?
(c) Hierarchically cluster the states using complete linkage and Eu- clidean distance, after scaling the variables to have standard de- viation one.
(d) What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed? Provide a justification for your answer.
'

#### (a) ####

library(ISLR)

# Fitting a hierarchical clustering
hc.complete = hclust(
  dist(USArrests),
  method="complete"
)

# View summary
summary(hc.complete)
hc.complete

plot(hc.complete)

#### (b) ####

# Cutting the tree to have only three branches (clusters)
hc.cut.complete = cutree(hc.complete, 3)

# Number of observations in each class
table(hc.cut.complete)

hc.cut.complete

# Plotting dendrogram with abline cutting off at 3 clusters
par(mfrow=c(1,1))
plot(hc.complete)
abline(h=150, col="red")


#### (c) ####

# Scaling the features
USArrests.scaled = scale(USArrests)

# Fitting the scaled features
hc.complete.scaled = hclust(
  dist(USArrests.scaled),
  method="complete"
)

# Plotting dendrogram
plot(hc.complete.scaled)

#### (d) ####

# Cutting the tree to have only three branches (clusters)
hc.scaled.cut.complete = cutree(hc.complete.scaled, 3)

table(hc.scaled.cut.complete)

table(hc.scaled.cut.complete, hc.cut.complete)

' COMMENTS:
Scaling the features changes the results. It decreases the vertical axis.
It might be good to scale the features because if the features are in 
difference range, the model might weight features that have high values
since its a big number even though the value might not have an impact
on result.
'
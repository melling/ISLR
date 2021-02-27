####  ####

#### (a) ####

gene = read.csv("chapter10/Ch10Ex11.csv", header=FALSE)

dim(gene)

summary(gene)

# Calculating the correlation-based distance
gene.corr.dist = as.dist(1-cor(gene))

# Fitting hierarchiacal clustering
h.clust = hclust(
  gene.corr.dist
)

# Plotting a dendrogram
plot(h.clust)

' COMMENTS:
Based on the dendrogram complete linkage seems like it can be 
classified into two groups.
'

# Fitting hierarchiacal clustering
h.clust = hclust(
  gene.corr.dist,
  method='average'
)

# Plotting a dendrogram
plot(h.clust)

' COMMENTS:
Average Linkage does not separate into two groups
'

# Fitting hierarchiacal clustering
h.clust = hclust(
  gene.corr.dist,
  method='single'
)

# Plotting a dendrogram
plot(h.clust)

' COMMENTS:
Single linkage looks like it can separate into two groups but looks 
wacky.
'

# Fitting hierarchiacal clustering
h.clust = hclust(
  gene.corr.dist,
  method='centroid'
)

# Plotting a dendrogram
plot(h.clust)

summary(h.clust)
h.clust

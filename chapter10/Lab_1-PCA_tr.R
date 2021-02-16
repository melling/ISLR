#### 10.4 Lab 1: Principal Components Analysis ####

# Collecting the row names
states = row.names(USArrests)
states

# Viewing the column names
names(USArrests)

# Applying mean to the data
apply(x=USArrests, MARGIN=2, FUN=mean) # MARGIN: axis to apply FUN

'COMMENTS: 
- 3x more rape than murders
- 8x more assults than rapes
'

# Applying variance to the data
apply(USArrests, 2, var)

'COMMENTS:
Huge differences in variances. Need to scale before performing PCA.
'

# Performing PCA
pr.out = prcomp(
  USArrests,
  scale=TRUE # Scale the features
)

# Viewing the results of pca
names(pr.out)

# Viewing the center (mean) of the PCA
pr.out$center

# Viewing the scale (std) of PCA 
pr.out$scale

# Viewing rotation, the PC loadings
pr.out$rotation

' COMMENTS:
Each column contains the corresponding PC loading vector.

There are 4 distinct PC because in general, there are MINIMUM(n-1, p) 
informative PC
'

# Viewing the dimensions of pr.out$x
dim(pr.out$x)

' COMMENTS:
$x has the PC score vectors: kth column is the kth PC score vector
'

# Plotting the first two PC
biplot(
  pr.out, 
  scale=0 # scales arrows to represent loadings
)

# Getting the std of PC
pr.out$sdev

# Calculating the variance explained
pr.var = pr.out$sdev^2
pr.var

# Calculating the proportion of variance explained
pve = pr.var/sum(pr.var)
pve
' COMMENTS:
First PC explains 62% of the variance in data
'

# Plotting the PVE
plot(
  pve, 
  xlab="Principal Component",
  ylab="Proportion of Variance Explained",
  ylim=c(0,1),
  type='b'
)

# Plotting the cumulative sum of PVE
plot(
  cumsum(pve),
  xlab="Principal Component",
  ylab="Cumulative Proportion of Variance Explained",
  ylim=c(0,1),
  type="b"
)

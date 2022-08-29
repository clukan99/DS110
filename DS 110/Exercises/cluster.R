# Cluster Analysis

data("USArrests") # Very old data
df <- data.frame(USArrests)
?USArrests # Corrections irrelevant to scaled cluster

df <- data.frame(scale(USArrests))
head(df)

#######################################################################

# k - Means Clustering
?kmeans

res.km <- kmeans(df, 4, nstart = 25) #nstarts fixes problems of differences by initial centroid guesses
print (res.km)

# We can ger the cluster a number to add to data frame
head (data.frame ("cluster" = res.km$cluster)) # we can output cluster as a new variable


library("factoextra")

# Enhanced k-means clustering - same algorithm nicer output.
res.km <- eclust(df, "kmeans", k=3, graph = TRUE) # no k uses gap

########  Optimal number of Clusters for setting K

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_silhouette(res.km) # silhouette plot


# Gap statistic
# nboot = 100 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
fviz_nbclust(df, kmeans,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

###### End optimal Clusters

#### Alternative Method to select number of clusters

library(NbClust)
NbClust(data = df, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

##########################################################

# Hierarchical Clustering

# Compute the dissimilarity / distance matrix
res.dist <- dist(df, method = "euclidean") # (n*(n-1)/2)

# Visualize the dissimilarity matrix
fviz_dist(res.dist, lab_size = 8)


# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")
print (res.hc)

# Visualize
plot(res.hc, cex = 0.5)

# Enhanced hierarchical clustering - same algorithm nicer output.
res.hc <- eclust(df, "hclust", k= 10, graph=FALSE, hc_metric="euclidean", hc_method="ward.D2")
fviz_dend(res.hc) # dendrogram
fviz_silhouette(res.hc) # silhouette plot

########  Optimal number of Clusters for setting K

# Elbow method
fviz_nbclust(df, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 100 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
fviz_nbclust(df, hcut, nstart = 25,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

###### End optimal Clusters


library(NbClust)

NbClust(data = df, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "ward.D2")


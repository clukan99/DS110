#Let's cluster iris

data("iris")
df <- data.frame(iris)

dfScale <- data.frame(scale(df[,-5])) # scale and remove Species

res.km <- kmeans(dfScale, 3, nstart = 25) #nstarts fixes problems of differences by initial centroid guesses
print (res.km)

# We can ger the cluster a number to add to data frame
head (data.frame ("cluster" = res.km$cluster)) # we can output cluster as a new variable


library("factoextra")

# Enhanced k-means clustering - same algorithm nicer output.
res.km <- eclust(dfScale, "kmeans", k=3, graph = TRUE) # no k uses gap

########  Optimal number of Clusters for setting K

# Elbow method
fviz_nbclust(dfScale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(dfScale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_silhouette(res.km) # silhouette plot


# Gap statistic
# nboot = 100 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
fviz_nbclust(dfScale, kmeans,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

###### End optimal Clusters

#### Alternative Method to select number of clusters

library(NbClust)
NbClust(data = dfScale, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

#Let's see how it does...

dfTest <- data.frame("cluster" = res.km$cluster, "Truth" = as.integer(df$Species))
table (dfTest)

library (caret)
confusionMatrix(factor(dfTest$cluster), reference=factor(dfTest$Truth)) 


##########################################################

# Hierarchical Clustering

# Compute the dissimilarity / distance matrix
res.dist <- dist(dfScale, method = "euclidean") # (n*(n-1)/2)

# Visualize the dissimilarity matrix
fviz_dist(res.dist, lab_size = 8)


# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")
print (res.hc)

# Visualize
plot(res.hc, cex = 0.5)

# Enhanced hierarchical clustering - same algorithm nicer output.
res.hc <- eclust(dfScale, "hclust", k=2, graph=FALSE, hc_metric="euclidean", hc_method="ward.D2")
fviz_dend(res.hc) # dendrogram
fviz_silhouette(res.hc) # silhouette plot

########  Optimal number of Clusters for setting K

# Elbow method
fviz_nbclust(dfScale, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(dfScale, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 100 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
fviz_nbclust(dfScale, hcut, nstart = 25,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

###### End optimal Clusters


library(NbClust)

NbClust(data = dfScale, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "ward.D2")

dfTest <- data.frame("cluster" = res.hc$cluster, "Truth" = as.integer(df$Species))
table (dfTest)

library (caret)
confusionMatrix(factor(dfTest$cluster), reference=factor(dfTest$Truth)) 



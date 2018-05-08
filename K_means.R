############################# K-means #######################################
#############################################################################

df <- USArrests
head(df)

# removing missing values, standardization
df <- na.omit(df)
nrow(df)

df <- sapply(df, function(x) scale(x))
df

## Distance or dissimilarity matrix

# function to visualize
fviz <- function(x)
{
fviz_dist(x, 
          gradient = list(low="#00AFBB",mid="white",high="#FC4E07"))
}

dist.euc <- get_dist(df)
dist.man <- get_dist(df, method = "manhattan")

fviz(dist.man)

# the K-means, which is implemented using
# Euclidean distance metric gives best result and K-means
# based on Manhattan distance metric's performance, is worst

# k-means

k.2 <- kmeans(df, centers = 2, nstart = 25)
str(k.2)

# total sum of squares
k.2$totss

# within cluster sum of squares (one per cluster)
k.2$withinss
tot.wit <- sum(k.2$withinss)

# between clusters
k.2$totss - sum(k.2$withinss)
k.2$betweenss

fviz_cluster(k.2, data = df)

k.4 <- kmeans(df, centers = 4, nstart = 25)
fviz_cluster(k.4, data = df, geom = "point")

## Determine optimal clusters

wss <- function(x) {
      return(kmeans(df, x, nstart = 25)$withinss) 
}
wss_values <- c()
k_values <- 1:10
for (i in 1:10)
{
  wss_values[i] <- wss(i)
  i=i+1
}

################################## Elbow graph ##############################
plot(k_values,wss_values, type = "b", pch = 19)
# cluster = 4
set.seed(931992)
fviz_nbclust(df,kmeans, method = "wss")


############################### Silhouette method ############################
#- quality of clustering (how well each object lies within its cluster)
#- high average silhouette width - good clustering


avg_sil <- function(x)
{
  km <- kmeans(df, x, nstart = 25)
  ss <- silhouette(km$cluster,dist(df))
  return(mean(ss[,3]))
}

k_values <- 2:15
sil <- c()
for (i in 2:15)
{
  sil[i] <- avg_sil(i)
  i=i+1
}

plot(1:15,sil, type = "b", pch = 19)
# cluster 2 and 4 - max avg silhouette values
fviz_nbclust(df, kmeans, method = "silhouette")

########################## gap statistics ###################################
set.seed(123)

gap_stat <- clusGap(df, kmeans, nstart = 25, K.max = 10, B=50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)


#############################################################################
############################# Hierarchical###################################
#############################################################################


library(cluster)
# types: agglomerative and divisive
# 1. AGNES: bottom-up; 2.DIANA: top-down (at each step, most heterogeneous cluster is divided into 2)
#
# Dissimilarity between 2 clusters of observations: linkage methods
# All pairwise dissimilarities between the elements in cluster 1 and 2 are calculated, 
# then average, smallest, maximum are considered.

# ward's minimum variance: minimize the total within-cluster variance.
# pair of clusters with minimum between-cluster distance are merged.

#agnes
hc2 <- agnes(df, method = "ward")
hc2$ac  # agglomerative coefficient (value closer to 1 suggest strong clustering)

pltree(hc2, cex=0.6, hang = -1)

# DIANA method
di <- diana(df)
di$dc

# tree formation
pltree(di, hang = -1)

# rectangular clusters
rect.hclust(as.hclust(hc2),k=4)

########################### Elbow method ###############################

fviz_nbclust(df, FUN = hcut, method = "wss")
fviz_nbclust(df, FUN = hcut, method = "silhouette")

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B  = 50)
fviz_gap_stat(gap_stat)


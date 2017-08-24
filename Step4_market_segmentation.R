# Our goal here is to determine the lending patterns in different geographies
# So I chose to conduct a county-level analysis for geographic segmentation

#Hypothesis: the lending pattern varies by the product_segment, Loan_Purpose_Description, Loan_Bucket, Conforming_Limit_000, FFIEC_Median_Family_Income

merged_data$County_Name <- as.character(merged_data$County_Name)
geographicTrends <- merged_data[which(merged_data$County_Name != ""),]
geographicTrends <- geographicTrends[!is.na(geographicTrends$Conforming_Limit_000),]
geographicTrends <- geographicTrends[which(geographicTrends$FFIEC_Median_Family_Income != "NA"),]

#preparing data for analysis by aggregating data on county level
geographicTrends <- geographicTrends %>%
  group_by(County_Name, product_segment, Loan_Purpose_Description, Loan_buckets, Conforming_Limit_000, FFIEC_Median_Family_Income) %>%
  summarise(total.loans = n()) %>%
  spread(County_Name, total.loans)

#removing NA values
geographicTrends[is.na(geographicTrends)] <- 0 

# Some counties have larger loans than others. In order to avoid problem in k-means, need some kind of normalize the quantity
# Convert total loan quantity for each county to percentage of total quantity
geographicTrends_mat <- as.matrix(geographicTrends[,-(1:5)])  # Drop first four columns
geographicTrends_mat <- prop.table(geographicTrends_mat, margin = 2)  # column-wise pct
geographicTrends_mat <- as.data.frame(geographicTrends_mat)
names(geographicTrends_mat) <- paste0('C',names(geographicTrends_mat))
geographicTrends <- bind_cols(geographicTrends[,(1:5)], geographicTrends_mat)

#k-means clustering
kmeans_data <- geographicTrends[,-(1:5)]  # Extract only county columns
kmeans_data_t <- t(kmeans_data)  # Get county in rows and products in columns

# Setup for k-means loop 
out_km <- list()
out_sil <- list()
x <- vector()
y <- vector()
minClust <- 4      # Hypothesized minimum number of segments 4 for 4 product_category
maxClust <- 10      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (clust in minClust:maxClust) {
  i <- clust-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  out_km[i] <- list(kmeans(kmeans_data_t, centers = clust, nstart = 50))
  out_sil[i] <- list(silhouette(out_km[[i]][[1]], dist(kmeans_data_t)))
  # Used for plotting silhouette average widths
  x[i] = clust  # value of k
  y[i] = summary(out_sil[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")


#### Get county names present in individual clusters ###

# Get attributes of optimal k-means output
maxSil <- which.max(y)          # Row number of max silhouette value
optimum_Clusters <- x[maxSil]    # Number of clusters
out_km_best <- out_km[[maxSil]] # k-means output of best cluster

# Creating list of customer names for each cluster
cluster_names <- list()
cluster_list <- list()
for (clust in 1:optimum_Clusters) {
  cluster_names[clust] <- paste0("X", clust)
  cluster_list[clust] <- list(
    names(
      out_km_best$cluster[out_km_best$cluster == clust]
    )
  )
}

names(cluster_list) <- cluster_names

print(cluster_list)

# Combine cluster centroids with bike models for feature inspection
Cluster_Centers <- as.data.frame(t(out_km_best$centers))  # Get centroids for groups
colnames(Cluster_Centers) <- paste0('X',1:optimum_Clusters)
geographicTrends_clustered <- bind_cols(geographicTrends[,1:5], Cluster_Centers)

# Analysing individual clusters
head(geographicTrends_clustered[order(-geographicTrends_clustered$X1), c(1:5, 6)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X2), c(1:5, 7)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X3), c(1:5, 8)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X4), c(1:5, 9)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X5), c(1:5, 10)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X6), c(1:5, 11)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X7), c(1:5, 12)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X8), c(1:5, 13)], 10)
head(geographicTrends_clustered[order(-geographicTrends_clustered$X9), c(1:5, 14)], 10)

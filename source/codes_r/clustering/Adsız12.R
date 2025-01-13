# Compute distance matrix
distance_matrix <- dist(clustering_data, method = "euclidean")

# Perform hierarchical clustering
hclust_result <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")

# Cut the tree into 3 clusters (k = 3)
hierarchical_clusters <- cutree(hclust_result, k = 3)

# Add cluster assignments to the dataset
clr_group$hierarchical_cluster <- as.factor(hierarchical_clusters)

# PCA for visualization
library(ggplot2)
pca <- prcomp(clustering_data, scale. = TRUE)
pca_data <- data.frame(pca$x[, 1:2], Cluster = clr_group$hierarchical_cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "Hierarchical Clustering Visualization (PCA)", x = "PC1", y = "PC2") +
  theme_minimal()

library(fpc)
stability_hierarchical <- clusterboot(
  distance_matrix,
  B = 50,
  bootmethod = "boot",
  clustermethod = hclustCBI,
  k = 3,
  method = "ward.D2"
)

print(stability_hierarchical)


cluster_summary <- clr_group %>%
  group_by(hierarchical_cluster) %>%
  summarize(across(starts_with("clr_"), mean, .names = "mean_{.col}"))
print(cluster_summary)

# Ensure formatted_date is of Date class
clr_group$formatted_date <- as.Date(clr_group$formatted_date)

ggplot(clr_group, aes(x = formatted_date, fill = hierarchical_cluster)) +
  geom_bar() +
  scale_x_date(
    date_breaks = "3 months",  # Breaks every 3 months
    date_labels = "%b %Y"     # Format as "Month Year" (e.g., "Jan 2023")
  ) +
  labs(title = "Hierarchical Clusters Over Time", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate the x-axis labels for better readability


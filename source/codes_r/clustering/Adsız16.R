library(fpc)
library(gridExtra)
library(grid)
library(dplyr)

# Define parameters
k_values <- c(2, 3, 4)  # Different k values
b_values <- c(50, 200, 500)  # Different bootstrap iterations

# Create an empty data frame to store results
stability_results <- data.frame()

# Loop through B and k values
for (B in b_values) {
  for (k in k_values) {
    # Hierarchical clustering
    set.seed(123)  # For reproducibility
    stability_hierarchical <- clusterboot(
      dist(clustering_data, method = "euclidean"),
      B = B,
      bootmethod = "boot",
      clustermethod = hclustCBI,
      k = k,
      method = "ward.D2"
    )
    
    # K-Means clustering
    set.seed(123)  # For reproducibility
    stability_kmeans <- clusterboot(
      clustering_data,
      B = B,
      bootmethod = "boot",
      clustermethod = kmeansCBI,
      k = k
    )
    
    # Extract cluster-level stability scores
    hierarchical_cluster_scores <- stability_hierarchical$bootmean
    kmeans_cluster_scores <- stability_kmeans$bootmean
    
    # Create a single row for this combination of B and k
    row_hier <- data.frame(
      B = B,
      k = k,
      Method = "Hierarchical Clustering",
      Cluster_1 = ifelse(k >= 1, hierarchical_cluster_scores[1], NA),
      Cluster_2 = ifelse(k >= 2, hierarchical_cluster_scores[2], NA),
      Cluster_3 = ifelse(k >= 3, hierarchical_cluster_scores[3], NA),
      Cluster_4 = ifelse(k >= 4, hierarchical_cluster_scores[4], NA),
      Summary = mean(hierarchical_cluster_scores, na.rm = TRUE)
    )
    row_kmeans <- data.frame(
      B = B,
      k = k,
      Method = "K-Means Clustering",
      Cluster_1 = ifelse(k >= 1, kmeans_cluster_scores[1], NA),
      Cluster_2 = ifelse(k >= 2, kmeans_cluster_scores[2], NA),
      Cluster_3 = ifelse(k >= 3, kmeans_cluster_scores[3], NA),
      Cluster_4 = ifelse(k >= 4, kmeans_cluster_scores[4], NA),
      Summary = mean(kmeans_cluster_scores, na.rm = TRUE)
    )
    
    # Append results to the data frame
    stability_results <- rbind(stability_results, row_hier, row_kmeans)
  }
}

# Highlight rows where k = 3
highlight_k3 <- stability_results$k == 3

# Custom table theme for highlighting
table_theme <- ttheme_default(
  core = list(
    bg_params = list(
      fill = ifelse(highlight_k3, "lightblue", "white"),  # Highlight k=3 rows
      col = NA
    ),
    fg_params = list(fontface = "plain")
  ),
  colhead = list(fg_params = list(fontface = "bold"))
)

# Save the results as a table in PNG
png("highlighted_k3_with_cluster4_table.png", width = 1400, height = 800)
grid.table(stability_results, theme = table_theme)
dev.off()

library(fpc)

# For hierarchical clustering
stability_hierarchical <- clusterboot(
  dist(clustering_data, method = "euclidean"), 
  B = 50, 
  bootmethod = "boot", 
  clustermethod = hclustCBI, 
  k = 3, 
  method = "ward.D2"
)

print(stability_hierarchical)

# For k-means clustering
stability_kmeans <- clusterboot(
  clustering_data, 
  B = 50, 
  bootmethod = "boot", 
  clustermethod = kmeansCBI, 
  k = 3
)

print(stability_kmeans)

ggplot(clr_group, aes(x = formatted_date, y = as.factor(cluster), color = as.factor(cluster))) +
  geom_point() +
  labs(x = "Month", y = "Cluster", color = "Cluster") +
  theme_minimal()



ggplot(clr_group, aes(x = formatted_date, fill = as.factor(cluster))) +
  geom_bar() +
  labs(x = "Month", y = "Number of Observations", fill = "Cluster") +
  theme_minimal()

clr_group <- read.csv("clr_group.csv")

# Calculate the mean CLR-transformed values for each variable by cluster
cluster_summary <- clr_group %>%
  group_by(cluster) %>%
  summarize(across(starts_with("clr_"), mean))

# View the summary
print(cluster_summary)


clustering_data <- clr_group[, grep("^clr_", colnames(clr_group))]

library(factoextra)

# Compute and plot within-cluster sum of squares (WCSS) for k = 1 to 10
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")

set.seed(123)

# k = 2
kmeans_2 <- kmeans(clustering_data, centers = 2)
clr_group$cluster_2 <- as.factor(kmeans_2$cluster)

# k = 3
kmeans_3 <- kmeans(clustering_data, centers = 3)
clr_group$cluster_3 <- as.factor(kmeans_3$cluster)

# k = 4
kmeans_4 <- kmeans(clustering_data, centers = 4)
clr_group$cluster_4 <- as.factor(kmeans_4$cluster)

library(dplyr)

# Summarize CLR variables by cluster for k = 2, 3, and 4
cluster_summary_2 <- clr_group %>%
  group_by(cluster_2) %>%
  summarize(across(starts_with("clr_"), mean))

cluster_summary_3 <- clr_group %>%
  group_by(cluster_3) %>%
  summarize(across(starts_with("clr_"), mean))

cluster_summary_4 <- clr_group %>%
  group_by(cluster_4) %>%
  summarize(across(starts_with("clr_"), mean))

print(cluster_summary_2)
print(cluster_summary_3)
print(cluster_summary_4)


# Bar plots of clusters over time for k = 2, 3, and 4
ggplot(clr_group, aes(x = formatted_date, fill = cluster_4)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 4", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

# Bar plots of clusters over time for k = 2, 3, and 4
ggplot(clr_group, aes(x = formatted_date, fill = cluster_3)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 3", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

# Bar plots of clusters over time for k = 2, 3, and 4
ggplot(clr_group, aes(x = formatted_date, fill = cluster_2)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 2", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

# Repeat for cluster_3 and cluster_4...

library(reshape2)
# Melt the data to long format for heatmap
cluster_summary_long <- cluster_summary %>%
  pivot_longer(cols = starts_with("clr_"), names_to = "Variable", values_to = "Mean_CLR")

# Create heatmap
ggplot(cluster_summary_long, aes(x = Variable, y = as.factor(cluster_3), fill = Mean_CLR)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(x = "Variable", y = "Cluster", fill = "Mean CLR Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(clr_group, aes(x = formatted_date)) +
  geom_line(aes(y = comb_data_date$avg_stringency_weighted_avg, color = "Stringency Index"), size = 1) +
  geom_bar(aes(y = as.numeric(cluster_3) * 25, fill = cluster_3), stat = "identity", alpha = 0.5) +
  scale_y_continuous(
    name = "Stringency Index (0-100)",
    sec.axis = sec_axis(~./25, name = "Cluster Membership")
  ) +
  labs(
    title = "Clusters and Stringency Index Over Time",
    x = "Month",
    color = "Metric",
    fill = "Cluster"
  ) +
  theme_minimal()

library(reshape2)
cluster_melt <- melt(cluster_summary, id.vars = "cluster_3")

ggplot(cluster_melt, aes(x = variable, y = cluster_3, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Cluster Characteristics", x = "CLR Variables", y = "Cluster", fill = "Mean CLR Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


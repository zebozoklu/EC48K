# Generate cluster assignments for k = 1, 2, 3
clr_group$cluster_k2 <- as.factor(cutree(hclust_result, k = 2))
clr_group$cluster_k3 <- as.factor(cutree(hclust_result, k = 3))
clr_group$cluster_k4 <- as.factor(cutree(hclust_result, k = 4))

# Reshape data to long format for plotting
library(tidyr)
clr_group_long <- clr_group %>%
  pivot_longer(
    cols = c(cluster_k2, cluster_k3, cluster_k4),
    names_to = "k",
    values_to = "Cluster"
  )

# Update `k` for readability
clr_group_long$k <- factor(clr_group_long$k,
                           levels = c("cluster_k2", "cluster_k3", "cluster_k4"),
                           labels = c("k = 2", "k = 3", "k = 4"))

# Plot clusters over time for k = 2, 3, and 4
ggplot(clr_group_long, aes(x = formatted_date, fill = Cluster)) +
  geom_bar() +
  facet_wrap(~k, ncol = 1) +  # Separate plots for each k
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(title = "Hierarchical Clusters Over Time", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels





library(reshape2)
library(ggplot2)

# Assuming `clr_group` already has hierarchical cluster assignments for k = 3
clr_group$cluster_k3 <- as.factor(cutree(hclust_result, k = 3))

# Summarize mean values of CLR variables by cluster
cluster_summary <- clr_group %>%
  group_by(cluster_k3) %>%
  summarize(across(starts_with("clr_"), mean, .names = "mean_{.col}"))

# Melt the data for ggplot
cluster_melt <- melt(cluster_summary, id.vars = "cluster_k3", 
                     variable.name = "CLR_Variable", 
                     value.name = "Mean_Value")

# Create the heatmap
ggplot(cluster_melt, aes(x = CLR_Variable, y = cluster_k3, fill = Mean_Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(
    title = "Cluster Characteristics Heatmap (k = 3)",
    x = "CLR Variables",
    y = "Cluster",
    fill = "Mean CLR Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



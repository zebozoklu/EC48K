library(ggplot2)
library(patchwork)  # For combining plots

# Create individual plots
plot_k2 <- ggplot(clr_group, aes(x = formatted_date, fill = cluster_2)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 2", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

plot_k3 <- ggplot(clr_group, aes(x = formatted_date, fill = cluster_3)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 3", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

plot_k4 <- ggplot(clr_group, aes(x = formatted_date, fill = cluster_4)) +
  geom_bar() +
  labs(title = "Time Patterns for k = 4", x = "Month", y = "Count", fill = "Cluster") +
  theme_minimal()

# Combine the plots
combined_plot <- plot_k2 + plot_k3 + plot_k4 +
  plot_layout(ncol = 1)  # Arrange plots in one column

# Save the combined plot as a PNG file
ggsave("combined_clusters.png", plot = combined_plot, width = 8, height = 12)

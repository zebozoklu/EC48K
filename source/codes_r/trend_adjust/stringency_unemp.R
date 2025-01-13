# Scale stringency index by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(scaled_stringency = avg_stringency_weighted_avg / 100 * trend)

# Plot scaled stringency index alongside unemployment trend
scaled_plot <- ggplot(comb_data_date, aes(x = formatted_date)) +
  geom_line(aes(y = trend, color = "Unemployment Trend"), size = 1.5) +
  geom_line(aes(y = scaled_stringency, color = "Scaled Stringency Index"), size = 1.5) +
  scale_color_manual(values = c("Unemployment Trend" = "black", "Scaled Stringency Index" = "blue")) +
  labs(
    title = "Unemployment Trend and Scaled Stringency Index Over Time",
    subtitle = "Stringency Index scaled by the unemployment trend.",
    x = "Date (Year-Month)",
    y = "Metric Value (Scaled)",
    color = "Metrics"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Display the plot
print(scaled_plot)

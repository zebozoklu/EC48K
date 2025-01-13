# Ensure 'formatted_date' is treated as a factor for grouping
scaled_sector_data$formatted_date <- as.factor(scaled_sector_data$formatted_date)

ggplot() +
  # Grouped bars for scaled sector shares
  geom_bar(data = scaled_sector_data, aes(x = formatted_date, y = scaled_share, fill = sector), 
           stat = "identity", position = position_dodge(width = 0.8), alpha = 0.9, width = 0.7) +
  # Trend line overlay
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend, group = 1), color = "black", size = 1.5) +
  labs(title = "Unemployment Decomposition by Sectors (Grouped Bars)",
       subtitle = "Trend Line Overlaid on Grouped Bars for Sectors",
       x = "Date", y = "Trend-Scaled Contribution",
       fill = "Sector") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

trend_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_sector_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = sector)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c("sector_primary_scaled" = "red", 
               "sector_secondary_scaled" = "green", 
               "sector_tertiary_scaled" = "blue"),
    labels = c("Primary Sector", "Secondary Sector", "Tertiary Sector")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Economic Sectors",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Sector Contribution to Unemployment Rate",
    fill = "Economic Sector"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

trend_adjusted_plot <- trend_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.5, size = 1.5)

ggsave("trend_adjusted_plot.png", trend_adjusted_plot, width = 10, height = 6, dpi = 300)


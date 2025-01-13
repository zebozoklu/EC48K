comb_data_date <- read_csv("comb_data_date.csv")

# Scale private and public sector variables by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_private = share_Private * trend,
    scaled_public = share_Public * trend
  )

# Reshape only private and public sector-related variables
scaled_sector_data <- comb_data_date %>%
  select(formatted_date, scaled_private, scaled_public, trend) %>%
  pivot_longer(cols = starts_with("scaled_"), names_to = "sector", values_to = "scaled_share")

# Recalculate ymin and ymax for stacked bars
scaled_sector_data <- scaled_sector_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )


# Add readable labels for sectors
scaled_sector_data <- scaled_sector_data %>%
  mutate(
    sector_label = case_when(
      str_detect(sector, "private") ~ "Private Sector",
      str_detect(sector, "public") ~ "Public Sector"
    )
  ) %>%
  mutate(
    sector_label = factor(sector_label, levels = c("Private Sector", "Public Sector"))
  )


# Create the plot
sector_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_sector_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = sector_label)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c(
      "Private Sector" = "blue", 
      "Public Sector" = "green"
    ),
    labels = c(
      "Private Sector", 
      "Public Sector"
    )
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Sector",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Sector Contribution to Unemployment Rate",
    fill = "Sector"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
sector_adjusted_plot <- sector_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.4, size = 1.5)

# Save the plot
ggsave("sector_adjusted_plot.png", sector_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(sector_adjusted_plot)

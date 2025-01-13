# Scale worker profile variables by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_blue_collar_hs = share_blue_collar_hs * trend,
    scaled_blue_collar_ls = share_blue_collar_ls * trend,
    scaled_white_collar_hs = share_white_collar_hs * trend,
    scaled_white_collar_ls = share_white_collar_ls * trend
  )

# Reshape data for plotting
scaled_worker_data <- comb_data_date %>%
  select(formatted_date, scaled_blue_collar_hs, scaled_blue_collar_ls, scaled_white_collar_hs, scaled_white_collar_ls, trend) %>%
  pivot_longer(cols = starts_with("scaled_"), names_to = "worker_profile", values_to = "scaled_share")

# Calculate ymin and ymax for stacked bars
scaled_worker_data <- scaled_worker_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )

# Add readable labels for worker profiles
scaled_worker_data <- scaled_worker_data %>%
  mutate(
    profile_label = case_when(
      str_detect(worker_profile, "blue_collar_hs") ~ "Blue Collar (HS)",
      str_detect(worker_profile, "blue_collar_ls") ~ "Blue Collar (LS)",
      str_detect(worker_profile, "white_collar_hs") ~ "White Collar (HS)",
      str_detect(worker_profile, "white_collar_ls") ~ "White Collar (LS)"
    )
  )

# Plot
worker_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_worker_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = profile_label)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c(
      "Blue Collar (HS)" = "darkblue", 
      "Blue Collar (LS)" = "lightblue", 
      "White Collar (HS)" = "darkgreen", 
      "White Collar (LS)" = "lightgreen"
    ),
    labels = c("Blue Collar (HS)", "Blue Collar (LS)", "White Collar (HS)", "White Collar (LS)")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Worker Profiles",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Worker Profile Contribution to Unemployment Rate",
    fill = "Worker Profile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
worker_adjusted_plot <- worker_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.6, size = 1.5)

# Save the plot
ggsave("worker_adjusted_plot.png", worker_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(worker_adjusted_plot)

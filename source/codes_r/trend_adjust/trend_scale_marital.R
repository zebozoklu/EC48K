# Filter out rows with NA in the selected marital status variables
comb_data_date <- comb_data_date %>%
  filter(
    !is.na(share_marital_divorced),
    !is.na(share_marital_married),
    !is.na(share_marital_never_married)
  )

# Scale marital status variables by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_divorced = share_marital_divorced * trend,
    scaled_married = share_marital_married * trend,
    scaled_never_married = share_marital_never_married * trend
  )

# Reshape only marital status-related variables
scaled_marital_data <- comb_data_date %>%
  select(formatted_date, scaled_divorced, scaled_married, scaled_never_married, trend) %>%
  pivot_longer(cols = starts_with("scaled_"), names_to = "marital_status", values_to = "scaled_share")

# Recalculate ymin and ymax for stacked bars
scaled_marital_data <- scaled_marital_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )

# Add readable labels for marital status
scaled_marital_data <- scaled_marital_data %>%
  mutate(
    marital_label = case_when(
      str_detect(marital_status, "scaled_divorced") ~ "Divorced",
      str_detect(marital_status, "scaled_married") ~ "Married",
      str_detect(marital_status, "scaled_never_married") ~ "Never Married"
    )
  )

# Create the plot
marital_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_marital_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = marital_label)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c(
      "Divorced" = "blue", 
      "Married" = "green",
      "Never Married" = "red"
    )
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Marital Status",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Marital Status Contribution to Unemployment Rate",
    fill = "Marital Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
marital_adjusted_plot <- marital_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.4, size = 1.5)

# Save the plot
ggsave("marital_adjusted_plot.png", marital_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(marital_adjusted_plot)

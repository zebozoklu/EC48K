comb_data_date <- read_csv("comb_data_date.csv")

# Filter out rows with NA in the selected worker category variables
comb_data_date <- comb_data_date %>%
  filter(
    !is.na(share_self_employed),
    !is.na(share_paid_employment),
    !is.na(share_wage_earner)
  )

# Scale worker category variables by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_self_employed = share_self_employed * trend,
    scaled_paid_employment = share_paid_employment * trend,
    scaled_wage_earner = share_wage_earner * trend
  )

# Reshape only worker category-related variables
scaled_worker_data <- comb_data_date %>%
  select(formatted_date, 
         scaled_self_employed, scaled_paid_employment, scaled_wage_earner, trend) %>%
  pivot_longer(cols = starts_with("scaled_"), names_to = "worker_category", values_to = "scaled_share")

# Recalculate ymin and ymax for stacked bars
scaled_worker_data <- scaled_worker_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )

# Add readable labels for worker categories
scaled_worker_data <- scaled_worker_data %>%
  mutate(
    worker_label = case_when(
      str_detect(worker_category, "self_employed") ~ "Self Employed",
      str_detect(worker_category, "paid_employment") ~ "Paid Employment",
      str_detect(worker_category, "wage_earner") ~ "Wage Earner"
    )
  ) %>%
  mutate(
    worker_label = factor(worker_label, levels = c("Self Employed", "Paid Employment", "Wage Earner"))
  )


# Create the plot
worker_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_worker_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = worker_label)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c(
      "Self Employed" = "orange", 
      "Paid Employment" = "red",
      "Wage Earner" = "purple"
    ),
    labels = c(
      "Self Employed", 
      "Paid Employment",
      "Wage Earner"
    )
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Worker Categories",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Worker Category Contribution to Unemployment Rate",
    fill = "Worker Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
worker_adjusted_plot <- worker_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.4, size = 1.5)

# Save the plot
ggsave("worker_adjusted_plot.png", worker_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(worker_adjusted_plot)


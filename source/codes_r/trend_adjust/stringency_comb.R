
# Add lagged variables
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_stringency_lag1 = lag(scaled_stringency, 1),
    scaled_stringency_lag2 = lag(scaled_stringency, 2),
    scaled_stringency_lag3 = lag(scaled_stringency, 3)
  )

comb_data_date <- comb_data_date %>%
  mutate(
    scaled_stringency_lag1 = replace_na(scaled_stringency_lag1, 0),
    scaled_stringency_lag2 = replace_na(scaled_stringency_lag2, 0),
    scaled_stringency_lag3 = replace_na(scaled_stringency_lag3, 0)
  )


library(ggplot2)

# Prepare data for plotting
lagged_plot_data <- comb_data_date %>%
  select(formatted_date, trend, scaled_stringency, scaled_stringency_lag1, scaled_stringency_lag2, scaled_stringency_lag3) %>%
  pivot_longer(cols = starts_with("scaled_stringency"), names_to = "Metric", values_to = "Value")

# Plot
ggplot(lagged_plot_data, aes(x = formatted_date, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), color = "black", size = 1.5) +
  labs(
    title = "Unemployment Trend and Lagged Scaled Stringency Index Over Time",
    x = "Date (Year-Month)",
    y = "Metric Value (Scaled)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Correlation test for lagged variables
cor_results <- comb_data_date %>%
  summarize(
    corr_no_lag = cor(trend, scaled_stringency, use = "complete.obs"),
    corr_lag1 = cor(trend, scaled_stringency_lag1, use = "complete.obs"),
    corr_lag2 = cor(trend, scaled_stringency_lag2, use = "complete.obs"),
    corr_lag3 = cor(trend, scaled_stringency_lag3, use = "complete.obs")
  )

# Print correlation results
print(cor_results)

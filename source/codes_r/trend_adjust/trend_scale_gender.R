# Scale gender variables by the unemployment trend
comb_data_date$share_male <- comb_data_date$share_gender_male / (comb_data_date$share_gender_male + comb_data_date$share_gender_female)
comb_data_date$share_female <- comb_data_date$share_gender_female / (comb_data_date$share_gender_male + comb_data_date$share_gender_female)


# Reshape data for plotting
scaled_gender_data <- comb_data_date %>%
  select(formatted_date, male_scaled, female_scaled, trend) %>%
  pivot_longer(cols = starts_with("male") | starts_with("female"), 
               names_to = "gender", values_to = "scaled_share")

scaled_gender_data <- scaled_gender_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)), 
    ymax = cumsum(scaled_share)
  )

scale_fill_manual(
  values = c("male_scaled" = "lightblue", "female_scaled" = "pink"),
  labels = c("Male", "Female")
)

gender_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_gender_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = gender)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c("male_scaled" = "lightblue", "female_scaled" = "pink"),
    labels = c("Female", "Male")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Gender",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Gender Contribution to Unemployment Rate",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

gender_adjusted_plot <- gender_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.3, size = 1.5)

# Save the plot
ggsave("gender_adjusted_plot.png", gender_adjusted_plot, width = 10, height = 6, dpi = 300)

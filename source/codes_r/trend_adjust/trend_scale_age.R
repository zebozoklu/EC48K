library(stringr)


comb_data_date <- comb_data_date %>%
  mutate(
    share_under_25 = `share_age_<25` / rowSums(select(., starts_with("share_age_")))*trend,
    share_25_34 = `share_age_25-34` / rowSums(select(., starts_with("share_age_")))*trend,
    share_35_44 = `share_age_35-44` / rowSums(select(., starts_with("share_age_")))*trend,
    share_45_54 = `share_age_45-54` / rowSums(select(., starts_with("share_age_")))*trend,
    share_55_plus = `share_age_55+` / rowSums(select(., starts_with("share_age_")))*trend
  )

scaled_age_data <- comb_data_date %>%
  select(formatted_date, share_under_25, share_25_34, share_35_44, share_45_54, share_55_plus, trend) %>%
  pivot_longer(cols = starts_with("share_"), names_to = "age_bracket", values_to = "scaled_share")

scaled_age_data <- scaled_age_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )

scaled_age_data <- scaled_age_data %>%
  mutate(
    age_group = case_when(
      str_detect(age_bracket, "under_25") ~ "Under 25",
      str_detect(age_bracket, "25_34") ~ "25-34",
      str_detect(age_bracket, "35_44") ~ "35-44",
      str_detect(age_bracket, "45_54") ~ "45-54",
      str_detect(age_bracket, "55_plus") ~ "55+"
    )
  )



age_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_age_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = age_group)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c("Under 25" = "cyan", 
               "25-34" = "lightblue", 
               "35-44" = "pink", 
               "45-54" = "orange", 
               "55+" = "purple"),
    labels = c("Under 25", "25-34", "35-44", "45-54", "55+"),
    guide = guide_legend(override.aes = list(
      fill = c("cyan", "lightblue", "pink", "orange", "purple")
    ))
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Age Groups",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Age Group Contribution to Unemployment Rate",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
age_adjusted_plot <- age_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.4, size = 1.5)

# Save the plot
ggsave("age_adjusted_plot.png", age_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(age_adjusted_plot)

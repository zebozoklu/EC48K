# Scale education variables by the unemployment trend
comb_data_date <- comb_data_date %>%
  mutate(
    scaled_illiterate = share_education_illiterate * trend,
    scaled_literate_no_school = share_education_literate_no_school * trend,
    scaled_primary = share_education_primary * trend,
    scaled_middle = share_education_middle * trend,
    scaled_high_school = share_education_high_school * trend,
    scaled_vocational = share_education_vocational * trend,
    scaled_university_plus = share_education_university_plus * trend
  )

# Reshape only education-related variables
scaled_education_data <- comb_data_date %>%
  select(formatted_date, starts_with("scaled_illiterate"), 
         starts_with("scaled_literate_no_school"), 
         starts_with("scaled_primary"), 
         starts_with("scaled_middle"), 
         starts_with("scaled_high_school"), 
         starts_with("scaled_vocational"), 
         starts_with("scaled_university_plus"), 
         trend) %>%
  pivot_longer(cols = starts_with("scaled_"), names_to = "education_level", values_to = "scaled_share")

# Recalculate ymin and ymax for stacked bars
scaled_education_data <- scaled_education_data %>%
  group_by(formatted_date) %>%
  mutate(
    ymin = cumsum(lag(scaled_share, default = 0)),
    ymax = cumsum(scaled_share)
  )


# Add readable labels for education levels
scaled_education_data <- scaled_education_data %>%
  mutate(
    education_label = case_when(
      str_detect(education_level, "illiterate") ~ "Illiterate",
      str_detect(education_level, "literate_no_school") ~ "Literate (No School)",
      str_detect(education_level, "primary") ~ "Primary Education",
      str_detect(education_level, "middle") ~ "Middle School",
      str_detect(education_level, "high_school") ~ "High School",
      str_detect(education_level, "vocational") ~ "Vocational",
      str_detect(education_level, "university_plus") ~ "University+"
    )
  )

# Ensure `education_label` is a factor with ordered levels
scaled_education_data <- scaled_education_data %>%
  mutate(
    education_label = factor(
      education_label,
      levels = c(
        "Illiterate", 
        "Literate (No School)",
        "Primary Education",
        "Middle School",
        "High School",
        "Vocational",
        "University+"
      )
    )
  )

# Corrected Plot
education_adjusted_plot <- ggplot() +
  geom_rect(data = scaled_education_data,
            aes(xmin = formatted_date - 10, 
                xmax = formatted_date + 10, 
                ymin = ymin, ymax = ymax, fill = education_label)) +
  geom_line(data = comb_data_date, aes(x = formatted_date, y = trend), 
            color = "black", size = 1.5) +
  scale_fill_manual(
    values = c(
      "Illiterate" = "blue", 
      "Literate (No School)" = "red",
      "Primary Education" = "orange",
      "Middle School" = "green",
      "High School" = "yellow",
      "Vocational" = "pink",
      "University+" = "purple"
    )
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    title = "Trend-Adjusted Unemployment Rate Decomposed by Education Levels",
    subtitle = "Dashed lines indicate key COVID-19-related events.",
    x = "Date (Year-Month)", 
    y = "Trend-Adjusted Education Level Contribution to Unemployment Rate",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Add dashed lines for COVID-19 events
education_adjusted_plot <- education_adjusted_plot +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2020-04-03", "2020-11-20", "2021-03-01", "2021-04-29")), 
             linetype = "dashed", color = "black", alpha = 0.4, size = 1.5)

# Save the plot
ggsave("education_adjusted_plot.png", education_adjusted_plot, width = 10, height = 6, dpi = 300)

# Display the plot
print(education_adjusted_plot)

# Add the covid_period column using your preferred logic
clr_data <- clr_data %>%
  mutate(
    covid_period = ifelse(
      formatted_date >= as.Date("2020-03-01") & formatted_date <= as.Date("2021-12-31"),
      "During COVID",
      "Pre-COVID"
    )
  )

# Subset the data for blue-collar and white-collar employment categories
employment_data <- clr_data %>%
  select(formatted_date, clr_share_blue_collar_hs, clr_share_blue_collar_ls, 
         clr_share_white_collar_hs, clr_share_white_collar_ls, covid_period) %>%
  pivot_longer(
    cols = starts_with("clr_share"),
    names_to = "Employment_Type",
    values_to = "CLR_Value"
  )


# Subset the data for blue-collar and white-collar employment categories
employment_data <- clr_data %>%
  select(formatted_date, covid_period, 
         clr_share_blue_collar_hs, clr_share_blue_collar_ls, 
         clr_share_white_collar_hs, clr_share_white_collar_ls) %>%
  pivot_longer(
    cols = starts_with("clr_share"),
    names_to = "Employment_Type",
    values_to = "CLR_Value"
  )

# Perform t-tests for each employment category
employment_ttests <- employment_data %>%
  group_by(Employment_Type) %>%
  summarize(
    p_value = t.test(
      CLR_Value[covid_period == "Pre-COVID"], 
      CLR_Value[covid_period == "During COVID"]
    )$p.value,
    mean_pre_covid = mean(CLR_Value[covid_period == "Pre-COVID"], na.rm = TRUE),
    mean_during_covid = mean(CLR_Value[covid_period == "During COVID"], na.rm = TRUE)
  )

# View the results
print(employment_ttests)

employment_data$formatted_date <- as.Date(employment_data$formatted_date)


# Plot blue-collar and white-collar CLR trends with COVID highlighted
ggplot(employment_data, aes(x = formatted_date, y = CLR_Value, color = Employment_Type, group = Employment_Type)) +
  geom_line(linewidth = 1) +
  annotate(
    "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
    ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray"
  ) +
  labs(
    title = "CLR Trends for Blue-Collar and White-Collar Employment with COVID Highlighted",
    x = "Date", y = "CLR Value", color = "Employment Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




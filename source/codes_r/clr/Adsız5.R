# Subset CLR and absolute share data for education levels
education_combined_data <- clr_data %>%
  select(formatted_date, starts_with("clr_share_education")) %>%
  left_join(
    comb_data_date %>% select(formatted_date, starts_with("share_education")),
    by = "formatted_date"
  ) %>%
  pivot_longer(cols = starts_with("clr_share_education"), names_to = "Education_Level_CLR", values_to = "CLR_Value")

education_combined_data$formatted_date <- as.Date(education_combined_data$formatted_date)



ggplot(education_combined_data, aes(x = formatted_date, group = Education_Level_CLR)) +
  geom_line(aes(y = CLR_Value, color = Education_Level_CLR), linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  labs(title = "Education Level CLR Trends in Unemployment with COVID Highlighted",
       x = "Date", y = "CLR Value", color = "Education Level (CLR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add a COVID period indicator
education_combined_data <- education_combined_data %>%
  mutate(covid_period = ifelse(formatted_date >= as.Date("2020-03-01") & 
                                 formatted_date <= as.Date("2021-12-31"), "During COVID", "Pre-COVID"))

# Calculate mean CLR values for each education level and period
education_summary <- education_combined_data %>%
  group_by(Education_Level_CLR, covid_period) %>%
  summarise(mean_clr = mean(CLR_Value, na.rm = TRUE))

# Perform t-tests for each education level
education_ttests <- education_combined_data %>%
  group_by(Education_Level_CLR) %>%
  summarise(
    p_value = t.test(
      CLR_Value[covid_period == "Pre-COVID"],
      CLR_Value[covid_period == "During COVID"]
    )$p.value,
    mean_pre_covid = mean(CLR_Value[covid_period == "Pre-COVID"], na.rm = TRUE),
    mean_during_covid = mean(CLR_Value[covid_period == "During COVID"], na.rm = TRUE)
  )

education_ttests


# Bar plot for mean CLR values
ggplot(education_summary, aes(x = Education_Level_CLR, y = mean_clr, fill = covid_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean CLR Values by Education Level Pre- and During COVID",
       x = "Education Level", y = "Mean CLR Value", fill = "COVID Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


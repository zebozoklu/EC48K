# Merge CLR and absolute share data
age_combined_data <- clr_data %>%
  select(formatted_date, starts_with("clr_share_age")) %>%
  left_join(
    comb_data_date %>% select(formatted_date, starts_with("share_age")),
    by = "formatted_date"
  ) %>%
  pivot_longer(cols = starts_with("clr_share_age"), names_to = "Age_Group_CLR", values_to = "CLR_Value") %>%
  mutate(Absolute_Share = case_when(
    Age_Group_CLR == "clr_share_age_.25" ~ share_age_.25,
    Age_Group_CLR == "clr_share_age_25.34" ~ share_age_25.34,
    Age_Group_CLR == "clr_share_age_35.44" ~ share_age_35.44,
    Age_Group_CLR == "clr_share_age_45.54" ~ share_age_45.54,
    Age_Group_CLR == "clr_share_age_55." ~ share_age_55.
  ))

age_combined_data$formatted_date <- as.Date(age_combined_data$formatted_date)


ggplot(age_combined_data, aes(x = formatted_date, group = Age_Group_CLR)) +
  geom_line(aes(y = CLR_Value, color = Age_Group_CLR), linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  labs(title = "Age Group CLR Trends in Unemployment with COVID Highlighted",
       x = "Date", y = "CLR Value", color = "Age Group (CLR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

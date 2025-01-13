# Step 1: Prepare the Data
wage_data <- clr_data %>%
  select(formatted_date, 
         clr_share_paid_employment, 
         clr_share_self_employed, 
         clr_share_wage_earner) %>%
  pivot_longer(cols = starts_with("clr_share"),
               names_to = "Employment_Type",
               values_to = "CLR_Value") %>%
  mutate(covid_period = case_when(
    formatted_date < as.Date("2020-03-01") ~ "Pre-COVID",
    formatted_date >= as.Date("2020-03-01") & formatted_date <= as.Date("2021-12-31") ~ "During COVID",
    TRUE ~ "Post-COVID"
  ))

wage_data$formatted_date <- as.Date(wage_data$formatted_date)


# Step 2: Visualize CLR Trends
library(ggplot2)
ggplot(wage_data, aes(x = formatted_date, y = CLR_Value, color = Employment_Type, group = Employment_Type)) +
  geom_line(linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
  labs(title = "CLR Trends for Wage Earners and Related Categories with COVID Highlighted",
       x = "Date", y = "CLR Value", color = "Employment Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 3: T-Tests
wage_ttests <- wage_data %>%
  group_by(Employment_Type) %>%
  summarize(
    p_value = t.test(
      CLR_Value[covid_period == "Pre-COVID"], 
      CLR_Value[covid_period == "During COVID"]
    )$p.value,
    mean_pre_covid = mean(CLR_Value[covid_period == "Pre-COVID"], na.rm = TRUE),
    mean_during_covid = mean(CLR_Value[covid_period == "During COVID"], na.rm = TRUE)
  )

# View T-Test Results
print(wage_ttests)

# Optional: Step 4 - Overlay Absolute Shares
absolute_wage_data <- comb_data_date %>%
  select(formatted_date, share_paid_employment, share_self_employed, 
         share_unpaid_family_worker, share_wage_earner) %>%
  pivot_longer(cols = starts_with("share"),
               names_to = "Employment_Type_Absolute",
               values_to = "Absolute_Share")

# Combine CLR and Absolute Data
combined_wage_data <- wage_data %>%
  left_join(absolute_wage_data, by = "formatted_date")

ggplot(combined_wage_data, aes(x = formatted_date)) +
  geom_line(aes(y = CLR_Value, color = Employment_Type), linewidth = 1) +
  geom_line(aes(y = Absolute_Share, linetype = Employment_Type_Absolute), linewidth = 1, alpha = 0.7) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
  labs(title = "CLR and Absolute Shares for Wage Earners and Related Categories",
       x = "Date", y = "Value", color = "Employment Type (CLR)", linetype = "Employment Type (Absolute)") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Absolute Shares")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

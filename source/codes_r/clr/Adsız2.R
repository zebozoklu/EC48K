library(ggplot2)
library(tidyr)

# Subset for gender data
gender_data <- clr_data %>%
  select(formatted_date, clr_share_gender_female, clr_share_gender_male) %>%
  pivot_longer(cols = -formatted_date, names_to = "Category", values_to = "CLR_Value") %>%
  mutate(covid_period = ifelse(formatted_date >= as.Date("2020-03-01") & 
                                 formatted_date <= as.Date("2021-12-31"), "During COVID", "Pre-COVID"))

# Ensure formatted_date is a Date object
gender_data$formatted_date <- as.Date(gender_data$formatted_date)


# Plot comparison of gender in one panel
ggplot(gender_data, aes(x = formatted_date, y = CLR_Value, color = Category, group = Category)) +
  geom_line(linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  labs(title = "Gender Composition in Unemployment with COVID Highlighted",
       x = "Date", y = "CLR Value", color = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Pre-COVID and During COVID subsets
pre_covid <- gender_data %>% filter(covid_period == "Pre-COVID")
during_covid <- gender_data %>% filter(covid_period == "During COVID")

# Compare means for females
t.test(
  pre_covid$CLR_Value[pre_covid$Category == "clr_share_gender_female"],
  during_covid$CLR_Value[during_covid$Category == "clr_share_gender_female"]
)

# Compare means for males
t.test(
  pre_covid$CLR_Value[pre_covid$Category == "clr_share_gender_male"],
  during_covid$CLR_Value[during_covid$Category == "clr_share_gender_male"]
)


# Compare absolute unemployment trends
absolute_unemployment <- comb_data_date %>%
  select(formatted_date, share_gender_female, share_gender_male, unemployment_rate)

absolute_unemployment$formatted_date <- as.Date(absolute_unemployment$formatted_date)


ggplot(absolute_long, aes(x = formatted_date, y = Value, color = Gender, group = Gender)) +
  geom_line(linewidth = 1) +
  labs(title = "Absolute Unemployment Trends by Gender",
       x = "Date", y = "Share of Unemployed", color = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

absolute_long <- absolute_unemployment %>%
  pivot_longer(cols = c(share_gender_female, share_gender_male), names_to = "Gender", values_to = "Value")



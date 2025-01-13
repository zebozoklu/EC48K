library(ggplot2)
library(tidyr)

clr_group <- clr_group %>%
  left_join(comb_data_date %>% select(formatted_date, avg_stringency_weighted_avg), by = "formatted_date")


# Reshape data for plotting
gender_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, clr_gender_female, clr_gender_male) %>%
  pivot_longer(
    cols = starts_with("clr_gender"),
    names_to = "Gender",
    values_to = "CLR_Value"
  )


# Plot CLR values for gender
ggplot(gender_data, aes(x = formatted_date, y = CLR_Value, color = Gender, group = Gender)) + # Add group aesthetic
  geom_line(size = 1) +
  geom_line(aes(y = avg_stringency_weighted_avg / 100, color = "Stringency Index", group = 1), linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "CLR Value (Gender)",
    sec.axis = sec_axis(~.*100, name = "Stringency Index (0-100)")
  ) +
  labs(
    title = "Gender Subgroup: CLR Variables and Stringency Index Over Time",
    x = "Month",
    y = "CLR Value",
    color = "Metric"
  ) +
  theme_minimal()


# Filter data for female
female_data <- gender_data %>%
  filter(Gender == "clr_gender_female")

# Filter data for male
male_data <- gender_data %>%
  filter(Gender == "clr_gender_male")

# Calculate correlation for females
cor_test_female <- cor.test(female_data$CLR_Value, female_data$avg_stringency_weighted_avg)

# Calculate correlation for males
cor_test_male <- cor.test(male_data$CLR_Value, male_data$avg_stringency_weighted_avg)

# Combine results
list(Female = cor_test_female, Male = cor_test_male)

# Linear regression for CLR gender female
lm_female <- lm(CLR_Value ~ avg_stringency_weighted_avg,
                data = female_data)
summary(lm_female)

# Linear regression for CLR gender male
lm_male <- lm(CLR_Value ~ avg_stringency_weighted_avg,
              data = male_data)
summary(lm_male)

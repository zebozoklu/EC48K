library(ggplot2)
library(tidyr)

clr_group <- clr_group %>%
  mutate(avg_stringency_weighted_avg = comb_data_date$avg_stringency_weighted_avg)

age_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, starts_with("clr_age")) %>%
  pivot_longer(
    cols = starts_with("clr_age"),
    names_to = "Age_Group",
    values_to = "CLR_Value"
  )

# Plot CLR values for age groups
ggplot(age_data, aes(x = formatted_date, y = CLR_Value, color = Age_Group, group = Age_Group)) + 
  geom_line(size = 1) + 
  geom_line(aes(y = avg_stringency_weighted_avg / 100, linetype = "Stringency Index (0-100)"), size = 1, color = "blue") +
  scale_y_continuous(
    name = "CLR Value (Age Group)",
    sec.axis = sec_axis(~.*100, name = "Stringency Index (0-100)")
  ) +
  labs(
    title = "Age Subgroup: CLR Variables and Stringency Index Over Time",
    x = "Month",
    y = "CLR Value (Age Group)",
    color = "Age Group",
    linetype = NULL # Removes unwanted linetype label
  ) +
  theme_minimal()

# Correlation for Age Subgroup
age_corr <- age_data %>%
  group_by(Age_Group) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  ) %>%
  as.data.frame()

print(age_corr)


# Regression for Age Subgroup
# Extract tidy regression results
age_reg_tidy <- age_data %>%
  group_by(Age_Group) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2, 4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

print(age_reg_tidy)


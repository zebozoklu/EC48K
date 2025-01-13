# Prepare data for plotting
education_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, starts_with("clr_edu")) %>%
  pivot_longer(
    cols = starts_with("clr_edu"),
    names_to = "Education_Group",
    values_to = "CLR_Value"
  )

# Plot CLR values for education groups
ggplot(education_data, aes(x = formatted_date, y = CLR_Value, color = Education_Group, group = Education_Group)) +
  geom_line(size = 1) +
  geom_line(aes(y = avg_stringency_weighted_avg / 100, color = "Stringency Index"), linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "CLR Value (Education)",
    sec.axis = sec_axis(~.*100, name = "Stringency Index (0-100)")
  ) +
  labs(
    title = "Education Subgroup: CLR Variables and Stringency Index Over Time",
    x = "Month",
    y = "CLR Value",
    color = "Metric"
  ) +
  theme_minimal()


# Calculate correlation for each education group
education_corr <- education_data %>%
  group_by(Education_Group) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  )

print(education_corr)

# Perform linear regression for each education group
education_reg <- education_data %>%
  group_by(Education_Group) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2, 4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

print(education_reg)



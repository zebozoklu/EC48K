# Reshape data for Blue/White-collar, Low/High-skilled subgroups
work_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, 
         clr_blue_collar_hs, clr_blue_collar_ls, 
         clr_white_collar_hs, clr_white_collar_ls) %>%
  pivot_longer(
    cols = c(clr_blue_collar_hs, clr_blue_collar_ls, 
             clr_white_collar_hs, clr_white_collar_ls),
    names_to = "Work_Type",
    values_to = "CLR_Value"
  )
# Plot CLR values and Stringency Index over time
ggplot(work_data, aes(x = formatted_date, y = CLR_Value, color = Work_Type, group = Work_Type)) +
  geom_line(size = 1) +
  geom_line(aes(y = avg_stringency_weighted_avg / 100), color = "black", linetype = "dashed", size = 1) + # Set stringency index color to black
  scale_y_continuous(
    name = "CLR Value (Work Type)",
    sec.axis = sec_axis(~.*100, name = "Stringency Index (0-100)")
  ) +
  labs(
    title = "Worker Subgroups: CLR Variables and Stringency Index Over Time",
    x = "Month",
    color = "Worker Type",
    linetype = "Metric"
  ) +
  theme_minimal()


# Calculate correlations for each work type
work_corr <- work_data %>%
  group_by(Work_Type) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  )

print(work_corr)

# Perform regression analysis for each work type
work_reg <- work_data %>%
  group_by(Work_Type) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2,4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

print(work_reg)








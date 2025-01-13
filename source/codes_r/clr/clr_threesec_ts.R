# Reshape data for plotting
sector_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, starts_with("clr_sector")) %>%
  pivot_longer(
    cols = starts_with("clr_sector"),
    names_to = "Sector",
    values_to = "CLR_Value"
  )

# Plot CLR values for sectors and stringency index
ggplot(sector_data, aes(x = formatted_date, y = CLR_Value, color = Sector, group = Sector)) +
  geom_line(size = 1) +
  geom_line(aes(y = avg_stringency_weighted_avg / 100, color = "Stringency Index"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "CLR Value (Sector)",
    sec.axis = sec_axis(~.*100, name = "Stringency Index (0-100)")
  ) +
  labs(
    title = "Sector Subgroup: CLR Variables and Stringency Index Over Time",
    x = "Month",
    y = "CLR Value",
    color = "Metric"
  ) +
  theme_minimal()

# Calculate correlation for each sector with stringency index
sector_corr <- sector_data %>%
  group_by(Sector) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  )

print(sector_corr)

# Perform regression analysis for each sector
sector_reg <- sector_data %>%
  group_by(Sector) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2,4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

print(sector_reg)


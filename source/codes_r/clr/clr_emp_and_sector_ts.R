# Employment Type Analysis

# Reshape data for employment type
employment_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, starts_with("clr_paid_employment"), starts_with("clr_self_employed"), starts_with("clr_wage_earner")) %>%
  pivot_longer(
    cols = starts_with("clr_"),
    names_to = "Employment_Type",
    values_to = "CLR_Value"
  )

# Correlation analysis for employment type
employment_corr <- employment_data %>%
  group_by(Employment_Type) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  )

# Regression analysis for employment type
employment_reg <- employment_data %>%
  group_by(Employment_Type) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2, 4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

# Sector Type Analysis

# Reshape data for sector type
sector_data <- clr_group %>%
  select(formatted_date, avg_stringency_weighted_avg, starts_with("clr_Public"), starts_with("clr_Private")) %>%
  pivot_longer(
    cols = starts_with("clr_"),
    names_to = "Sector_Type",
    values_to = "CLR_Value"
  )

# Correlation analysis for sector type
sector_corr <- sector_data %>%
  group_by(Sector_Type) %>%
  summarise(
    Correlation = cor(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs"),
    P_Value = cor.test(CLR_Value, avg_stringency_weighted_avg, use = "complete.obs")$p.value
  )

# Regression analysis for sector type
sector_reg <- sector_data %>%
  group_by(Sector_Type) %>%
  summarise(
    Intercept = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[1],
    Slope = coef(lm(CLR_Value ~ avg_stringency_weighted_avg))[2],
    P_Value = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$coefficients[2, 4],
    R_Squared = summary(lm(CLR_Value ~ avg_stringency_weighted_avg))$r.squared
  )

# Print results for both subgroups
print(employment_corr)
print(employment_reg)
print(sector_corr)
print(sector_reg)

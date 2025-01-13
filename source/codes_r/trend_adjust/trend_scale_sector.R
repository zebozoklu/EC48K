# Extract trend component from STL decomposition
trend <- decomposition$time.series[, "trend"]
comb_data_date$trend <- trend



 # Scale sector shares by the unemployment rate
comb_data_date$sector_primary_scaled <- comb_data_date$share_sector_Primary * comb_data_date$trend
comb_data_date$sector_secondary_scaled <- comb_data_date$share_sector_Secondary * comb_data_date$trend
comb_data_date$sector_tertiary_scaled <- comb_data_date$share_sector_Tertiary * comb_data_date$trend

# Reshape data for grouped bar plot
scaled_sector_data <- comb_data_date %>%
  select(formatted_date, sector_primary_scaled, sector_secondary_scaled, sector_tertiary_scaled, trend) %>%
  pivot_longer(cols = starts_with("sector"), names_to = "sector", values_to = "scaled_share")


# Sort data by sector in the desired stacking order
scaled_sector_data <- scaled_sector_data %>%
  mutate(sector = factor(sector, levels = c("sector_primary_scaled", "sector_secondary_scaled", "sector_tertiary_scaled"))) %>%
  arrange(formatted_date, sector)
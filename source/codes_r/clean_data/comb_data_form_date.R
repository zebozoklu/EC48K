# Add the covid_period column to each dataset
pre_covid$covid_period <- "Pre-COVID"
covid_era$covid_period <- "COVID"

combined_data <- rbind(pre_covid, covid_era)

combined_data$covid_period_binary <- ifelse(combined_data$covid_period == "COVID", 1, 0)

# Convert full month names to numeric values
combined_data$month_numeric <- match(combined_data$month, month.name)

# Create a formatted date column
combined_data$formatted_date <- as.Date(
  paste(combined_data$year, combined_data$month_numeric, "01", sep = "-"),
  format = "%Y-%m-%d"
)

write_csv(combined_data, "comb_data_date.csv")

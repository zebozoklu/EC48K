# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the consistent labor force datasets
pre_covid <- read_csv("pre_covid.csv")
covid_era <- read_csv("covid_era.csv")

# Calculate unemployment rate for pre_covid dataset
pre_covid_unemployment <- pre_covid %>%
  group_by(year, month) %>%
  summarize(
    unemployment_rate = mean(main_activity_status == 5, na.rm = TRUE),
    .groups = "drop"  # Explicitly drop grouping
  )

# Calculate unemployment rate for covid_era dataset
covid_era_unemployment <- covid_era %>%
  group_by(year, month) %>%
  summarize(
    unemployment_rate = mean(main_activity_status == 5, na.rm = TRUE),
    .groups = "drop"  # Explicitly drop grouping
  )

# Define the order for months
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Add an order column to pre_covid_unemployment
pre_covid_unemployment <- pre_covid_unemployment %>%
  mutate(month_order = match(month, month_order)) %>%
  arrange(year, month_order)

# Add an order column to covid_era_unemployment
covid_era_unemployment <- covid_era_unemployment %>%
  mutate(month_order = match(month, month_order)) %>%
  arrange(year, month_order)

# Remove the temporary month_order column
pre_covid_unemployment <- pre_covid_unemployment %>%
  select(-month_order)

covid_era_unemployment <- covid_era_unemployment %>%
  select(-month_order)

# Write the cleaned datasets to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

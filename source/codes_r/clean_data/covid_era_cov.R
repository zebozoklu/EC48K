# Load necessary libraries
library(readr)
library(dplyr)

# Load the unemployment data and aggregated COVID datasets
covid_era_unemployment <- read_csv("covid_era_unemployment.csv")
monthly_cum_covid <- read_csv("monthly_cum_covid.csv")
monthly_week_covid <- read_csv("monthly_week_covid.csv")

# Load necessary libraries
library(dplyr)

# Load the aggregated datasets
monthly_cum_covid <- read_csv("monthly_cum_covid.csv")
monthly_week_covid <- read_csv("monthly_week_covid.csv")

# Standardize the 'month' column to full month names
monthly_cum_covid <- monthly_cum_covid %>%
  mutate(month = case_when(
    month == "Jan" ~ "January",
    month == "Feb" ~ "February",
    month == "Mar" ~ "March",
    month == "Apr" ~ "April",
    month == "May" ~ "May",
    month == "Jun" ~ "June",
    month == "Jul" ~ "July",
    month == "Aug" ~ "August",
    month == "Sep" ~ "September",
    month == "Oct" ~ "October",
    month == "Nov" ~ "November",
    month == "Dec" ~ "December",
    TRUE ~ month  # Handle unexpected cases
  ))

monthly_week_covid <- monthly_week_covid %>%
  mutate(month = case_when(
    month == "Jan" ~ "January",
    month == "Feb" ~ "February",
    month == "Mar" ~ "March",
    month == "Apr" ~ "April",
    month == "May" ~ "May",
    month == "Jun" ~ "June",
    month == "Jul" ~ "July",
    month == "Aug" ~ "August",
    month == "Sep" ~ "September",
    month == "Oct" ~ "October",
    month == "Nov" ~ "November",
    month == "Dec" ~ "December",
    TRUE ~ month  # Handle unexpected cases
  ))


# Merge cumulative COVID data with unemployment data
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(monthly_cum_covid, by = c("year", "month"))

# Merge weekly COVID data with unemployment data
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(monthly_week_covid, by = c("Entity", "year", "month"))

# Save the updated dataset
write_csv(covid_era_unemployment, "covid_era_unemployment_with_covid_data.csv")

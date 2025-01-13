# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the consistent labor force datasets
pre_covid <- read_csv("pre_covid.csv")
covid_era <- read_csv("covid_era.csv")

# Output directories
pre_covid_unemployment <- read_csv("pre_covid_unemployment.csv")
covid_era_unemployment <- read_csv("covid_era_unemployment.csv")

# Create marital status brackets and calculate shares for pre-COVID (unemployed only)
pre_covid_marital_status <- pre_covid %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    marital_status = case_when(
      FB100 == 1 ~ "married",
      FB100 == 2 ~ "never_married",
      FB100 == 3 ~ "widowed",
      FB100 == 4 ~ "divorced",
      FB100 == 5 ~ "separated"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, marital_status) %>%  # Further group by marital status
  summarize(
    share_in_status = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share of each marital status group
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = marital_status,
    values_from = share_in_status,
    names_prefix = "share_marital_"
  )

# Create marital status brackets and calculate shares for COVID era (unemployed only)
covid_era_marital_status <- covid_era %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    marital_status = case_when(
      FB100 == 1 ~ "married",
      FB100 == 2 ~ "never_married",
      FB100 == 3 ~ "widowed",
      FB100 == 4 ~ "divorced",
      FB100 == 5 ~ "separated"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, marital_status) %>%  # Further group by marital status
  summarize(
    share_in_status = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share of each marital status group
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = marital_status,
    values_from = share_in_status,
    names_prefix = "share_marital_"
  )

# Merge pre-COVID unemployment data with marital status shares
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_marital_status, by = c("year", "month"))

# Merge COVID-era unemployment data with marital status shares
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_marital_status, by = c("year", "month"))

# Write the updated datasets to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

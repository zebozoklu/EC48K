# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the consistent labor force datasets
pre_covid <- read_csv("pre_covid.csv")
covid_era <- read_csv("covid_era.csv")

# Output datasets for unemployment level
pre_covid_unemployment <- read_csv("pre_covid_unemployment.csv")
covid_era_unemployment <- read_csv("covid_era_unemployment.csv")

# Map job status codes to meaningful names
job_status_labels <- c(
  "1" = "paid_employment",
  "2" = "wage_earner",
  "3" = "employer",
  "4" = "self_employed",
  "5" = "unpaid_family_worker"
)

# Define a function to calculate job status shares at unemployment level
calculate_job_status_shares <- function(data) {
  data %>%
    filter(main_activity_status == 5, !is.na(FI120)) %>%  # Filter for unemployed individuals and exclude NAs
    mutate(FI120 = case_when(
      FI120 == 1 ~ "paid_employment",
      FI120 == 2 ~ "wage_earner",
      FI120 == 3 ~ "employer",
      FI120 == 4 ~ "self_employed",
      FI120 == 5 ~ "unpaid_family_worker",
      TRUE ~ NA_character_  # Handle unexpected cases
    )) %>%
    group_by(year, month) %>%  # Group by year and month
    mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed individuals
    group_by(year, month, FI120) %>%  # Group by job status
    summarize(
      share_in_status = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate shares
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = FI120,
      values_from = share_in_status,
      names_prefix = "share_"
    )
}

# Calculate job status shares for pre_covid and covid_era datasets
pre_covid_job_status <- calculate_job_status_shares(pre_covid)
covid_era_job_status <- calculate_job_status_shares(covid_era)

# Merge the results with unemployment datasets
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_job_status, by = c("year", "month"))

covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_job_status, by = c("year", "month"))

# Write the updated datasets to CSV
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")

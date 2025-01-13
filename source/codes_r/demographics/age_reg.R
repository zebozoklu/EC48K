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

# Filter for unemployed individuals and create age brackets, calculate shares for pre_covid
pre_covid_age_brackets <- pre_covid %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    age_bracket = case_when(
      FK070 < 25 ~ "<25",
      FK070 >= 25 & FK070 < 35 ~ "25-34",
      FK070 >= 35 & FK070 < 45 ~ "35-44",
      FK070 >= 45 & FK070 < 55 ~ "45-54",
      FK070 >= 55 ~ "55+"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed individuals per year-month
  group_by(year, month, age_bracket) %>%  # Group by age bracket
  summarize(
    share_in_bracket = n_distinct(PERS_ID) / first(total_unemployed),  # Share in each bracket
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = age_bracket, values_from = share_in_bracket, names_prefix = "share_age_")

# Filter for unemployed individuals and create age brackets, calculate shares for covid_era
covid_era_age_brackets <- covid_era %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    age_bracket = case_when(
      FK070 < 25 ~ "<25",
      FK070 >= 25 & FK070 < 35 ~ "25-34",
      FK070 >= 35 & FK070 < 45 ~ "35-44",
      FK070 >= 45 & FK070 < 55 ~ "45-54",
      FK070 >= 55 ~ "55+"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed individuals per year-month
  group_by(year, month, age_bracket) %>%  # Group by age bracket
  summarize(
    share_in_bracket = n_distinct(PERS_ID) / first(total_unemployed),  # Share in each bracket
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = age_bracket, values_from = share_in_bracket, names_prefix = "share_age_")

# Merge with pre_covid_unemployment
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_age_brackets, by = c("year", "month"))

# Merge with covid_era_unemployment
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_age_brackets, by = c("year", "month"))

# Write results to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

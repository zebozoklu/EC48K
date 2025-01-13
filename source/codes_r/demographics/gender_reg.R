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

# Map FK090 to Male/Female
pre_covid <- pre_covid %>%
  mutate(gender = case_when(
    FK090 == 1 ~ "male",
    FK090 == 2 ~ "female"
  ))

covid_era <- covid_era %>%
  mutate(gender = case_when(
    FK090 == 1 ~ "male",
    FK090 == 2 ~ "female"
  ))

# Filter for unemployed individuals and calculate gender shares for pre_covid dataset
pre_covid_gender <- pre_covid %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  group_by(year, month, gender) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  group_by(year, month) %>%
  mutate(
    share_gender = count / sum(count)  # Calculate gender shares
  ) %>%
  select(year, month, gender, share_gender) %>%
  pivot_wider(names_from = gender, values_from = share_gender, names_prefix = "share_gender_")

# Filter for unemployed individuals and calculate gender shares for covid_era dataset
covid_era_gender <- covid_era %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  group_by(year, month, gender) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  group_by(year, month) %>%
  mutate(
    share_gender = count / sum(count)  # Calculate gender shares
  ) %>%
  select(year, month, gender, share_gender) %>%
  pivot_wider(names_from = gender, values_from = share_gender, names_prefix = "share_gender_")

# Merge gender shares with pre_covid_unemployment
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_gender, by = c("year", "month"))

# Merge gender shares with covid_era_unemployment
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_gender, by = c("year", "month"))

# Write results to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

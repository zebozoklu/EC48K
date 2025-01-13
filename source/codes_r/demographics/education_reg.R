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

# Create education brackets and calculate shares for pre-COVID (unemployed only)
pre_covid_education <- pre_covid %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    education_bracket = case_when(
      FE030 == 0 ~ "illiterate",
      FE030 == 1 ~ "literate_no_school",
      FE030 == 2 ~ "primary",
      FE030 == 3 ~ "middle",
      FE030 == 4 ~ "high_school",
      FE030 == 5 ~ "vocational",
      FE030 == 6 ~ "university_plus"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, education_bracket) %>%  # Further group by education level
  summarize(
    share_in_education = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share of each education group
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = education_bracket,
    values_from = share_in_education,
    names_prefix = "share_education_"
  )

# Create education brackets and calculate shares for COVID-era (unemployed only)
covid_era_education <- covid_era %>%
  filter(main_activity_status == 5) %>%  # Filter for unemployed individuals
  mutate(
    education_bracket = case_when(
      FE030 == 0 ~ "illiterate",
      FE030 == 1 ~ "literate_no_school",
      FE030 == 2 ~ "primary",
      FE030 == 3 ~ "middle",
      FE030 == 4 ~ "high_school",
      FE030 == 5 ~ "vocational",
      FE030 == 6 ~ "university_plus"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month for total unemployed
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, education_bracket) %>%  # Further group by education level
  summarize(
    share_in_education = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share of each education group
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = education_bracket,
    values_from = share_in_education,
    names_prefix = "share_education_"
  )

# Merge pre-COVID unemployment data with education shares
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_education, by = c("year", "month"))

# Merge COVID-era unemployment data with education shares
covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_education, by = c("year", "month"))

# Write results to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

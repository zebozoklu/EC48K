# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the consistent labor force datasets
pre_covid <- read_csv("pre_covid.csv")
covid_era <- read_csv("covid_era.csv")

# output dir
pre_covid_unemployment <- read_csv("pre_covid_unemployment.csv")
covid_era_unemployment <- read_csv("covid_era_unemployment.csv")

# Apply updated categories for pre_covid
pre_covid_occupation <- pre_covid %>%
  filter(main_activity_status == 5, !is.na(FI130)) %>%  # Filter for unemployed individuals
  mutate(
    FI130_broad = case_when(
      FI130 == 1 ~ "white_collar_hs",
      FI130 == 2 ~ "white_collar_hs",
      FI130 == 3 ~ "white_collar_hs",
      FI130 == 4 ~ "white_collar_ls",
      FI130 == 5 ~ "white_collar_ls",
      FI130 == 6 ~ "blue_collar_hs",
      FI130 == 7 ~ "blue_collar_hs",
      FI130 == 8 ~ "blue_collar_ls",
      FI130 == 9 ~ "blue_collar_ls",
      TRUE ~ NA_character_  # Handle unexpected cases
    )
  ) %>%
  group_by(year, month) %>%
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year/month
  group_by(year, month, FI130_broad) %>%
  summarize(
    share_in_broad_occupation = n_distinct(PERS_ID) / first(total_unemployed),  # Share of unemployed by occupation
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = FI130_broad,
    values_from = share_in_broad_occupation,
    names_prefix = "share_"
  )


# Apply updated categories for covid_era
covid_era_occupation <- covid_era %>%
  filter(main_activity_status == 5, !is.na(FI130)) %>%  # Filter for unemployed individuals
  mutate(
    FI130_broad = case_when(
      FI130 == 1 ~ "white_collar_hs",
      FI130 == 2 ~ "white_collar_hs",
      FI130 == 3 ~ "white_collar_hs",
      FI130 == 4 ~ "white_collar_ls",
      FI130 == 5 ~ "white_collar_ls",
      FI130 == 6 ~ "blue_collar_hs",
      FI130 == 7 ~ "blue_collar_hs",
      FI130 == 8 ~ "blue_collar_ls",
      FI130 == 9 ~ "blue_collar_ls",
      TRUE ~ NA_character_  # Handle unexpected cases
    )
  ) %>%
  group_by(year, month) %>%
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year/month
  group_by(year, month, FI130_broad) %>%
  summarize(
    share_in_broad_occupation = n_distinct(PERS_ID) / first(total_unemployed),  # Share of unemployed by occupation
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = FI130_broad,
    values_from = share_in_broad_occupation,
    names_prefix = "share_"
  )

# Merge with unemployment datasets
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_occupation, by = c("year", "month"))

covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_occupation, by = c("year", "month"))



write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")



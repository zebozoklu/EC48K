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

# Define three-sector model categories explicitly
sector_model_labels <- c(
  "01" = "Primary",
  "02" = "Primary",
  "03" = "Secondary",
  "04" = "Secondary",
  "05" = "Secondary",
  "06" = "Tertiary",
  "07" = "Tertiary",
  "08" = "Tertiary",
  "09" = "Tertiary",
  "10" = "Tertiary",
  "11" = "Tertiary",
  "12" = "Tertiary",
  "13" = "Tertiary",
  "14" = "Tertiary",
  "15" = "Tertiary",
  "16" = "Tertiary",
  "17" = "Tertiary",
  "18" = "Tertiary"
)

# Apply three-sector model to pre_covid (filtering for unemployed individuals)
pre_covid_sector_three <- pre_covid %>%
  filter(main_activity_status == 5, !is.na(FI140)) %>%  # Filter for unemployed and valid sector codes
  mutate(sector = recode(as.character(FI140), !!!sector_model_labels)) %>%
  group_by(year, month) %>%
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, sector) %>%
  summarize(
    share_in_sector = n_distinct(PERS_ID) / first(total_unemployed),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = share_in_sector,
    names_prefix = "share_sector_"
  )

# Apply three-sector model to covid_era (filtering for unemployed individuals)
covid_era_sector_three <- covid_era %>%
  filter(main_activity_status == 5, !is.na(FI140)) %>%  # Filter for unemployed and valid sector codes
  mutate(sector = recode(as.character(FI140), !!!sector_model_labels)) %>%
  group_by(year, month) %>%
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed for each year and month
  group_by(year, month, sector) %>%
  summarize(
    share_in_sector = n_distinct(PERS_ID) / first(total_unemployed),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = share_in_sector,
    names_prefix = "share_sector_"
  )

# Merge the corrected sectors with unemployment data
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_sector_three, by = c("year", "month"))

covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_sector_three, by = c("year", "month"))

# Write the updated datasets to CSV
write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")

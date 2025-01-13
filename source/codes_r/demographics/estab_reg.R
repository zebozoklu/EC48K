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



# Pre-COVID: Calculate shares excluding NAs
pre_covid_establishment_type <- pre_covid %>%
  filter(main_activity_status == 5, !is.na(FI145)) %>%  # Filter for unemployed individuals and exclude NAs
  mutate(
    establishment_type = case_when(
      FI145 == 1 ~ "Private",
      FI145 == 2 ~ "Public"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed individuals
  group_by(year, month, establishment_type) %>%  # Group by establishment type
  summarize(
    share = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share for each type
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = establishment_type,
    values_from = share,
    names_prefix = "share_"
  )

# COVID-era: Same process
covid_era_establishment_type <- covid_era %>%
  filter(main_activity_status == 5, !is.na(FI145)) %>%  # Remove rows where FI145 is NA
  mutate(
    establishment_type = case_when(
      FI145 == 1 ~ "Private",
      FI145 == 2 ~ "Public"
    )
  ) %>%
  group_by(year, month) %>%  # Group by year and month
  mutate(total_unemployed = n_distinct(PERS_ID)) %>%  # Total unemployed individuals
  group_by(year, month, establishment_type) %>%  # Group by establishment type
  summarize(
    share = n_distinct(PERS_ID) / first(total_unemployed),  # Calculate share for each type
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = establishment_type,
    values_from = share,
    names_prefix = "share_"
  )


# Merge with unemployment datasets
pre_covid_unemployment <- pre_covid_unemployment %>%
  left_join(pre_covid_establishment_type, by = c("year", "month"))

covid_era_unemployment <- covid_era_unemployment %>%
  left_join(covid_era_establishment_type, by = c("year", "month"))







write_csv(covid_era_unemployment, "covid_era_unemployment.csv")
write_csv(pre_covid_unemployment, "pre_covid_unemployment.csv")





# Load necessary libraries
library(readr)
library(dplyr)

# Read the consistent labor force datasets
data_2018 <- read_csv("consistent_labor_force_2018.csv")
data_2019 <- read_csv("consistent_labor_force_2019.csv")
data_2020 <- read_csv("consistent_labor_force_2020.csv")
data_2021 <- read_csv("consistent_labor_force_2021.csv")

# Combine pre-COVID years
pre_covid <- bind_rows(
  data_2018 %>% mutate(year = 2018),
  data_2019 %>% mutate(year = 2019)
)

# Combine COVID-era years
covid_era <- bind_rows(
  data_2020 %>% mutate(year = 2020),
  data_2021 %>% mutate(year = 2021)
)

# Drop FK010 and year, keeping only FB010
pre_covid <- pre_covid %>%
  select(-FK010, -year)

covid_era <- covid_era %>%
  select(-FK010, -year)

# Rename FB010 to year in pre_covid
pre_covid <- pre_covid %>%
  rename(year = FB010)

# Rename FB010 to year in covid_era
covid_era <- covid_era %>%
  rename(year = FB010)

# Reorder columns for pre_covid dataset
pre_covid <- pre_covid %>%
  select(HH_ID, PERS_ID, year, month, main_activity_status, everything())

# Reorder columns for covid_era dataset
covid_era <- covid_era %>%
  select(HH_ID, PERS_ID, year, month, main_activity_status, everything())

pre_covid <- pre_covid %>% select(-stayed_in_labor_force, -in_labor_force)
covid_era <- covid_era %>% select(-stayed_in_labor_force, -in_labor_force)

# Add the covid_period column to each dataset
pre_covid$covid_period <- "Pre-COVID"
covid_era$covid_period <- "COVID"

combined_data <- rbind(pre_covid, covid_era)

combined_data$covid_period_binary <- ifelse(combined_data$covid_period == "COVID", 1, 0)

# Convert full month names to numeric values
combined_data$month_numeric <- match(combined_data$month, month.name)

# Create a formatted date column
combined_data$formatted_date <- as.Date(
  paste(combined_data$year, combined_data$month_numeric, "01", sep = "-"),
  format = "%Y-%m-%d"
)

write_csv(pre_covid, "pre_covid.csv")
write_csv(covid_era, "covid_era.csv")
write_csv(combined_data, "comb_data_date.csv")




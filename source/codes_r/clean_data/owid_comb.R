stringency_index <- read.csv("covid-19-stringency-index.csv")

stringency_index <- stringency_index %>%
  mutate(formatted_date = as.Date(Day))

stringency_index <- stringency_index %>%
  mutate(formatted_date = as.Date(format(formatted_date, "%Y-%m-01")))

complete_dates <- data.frame(
  formatted_date = seq.Date(
    from = as.Date("2018-01-01"), # Adjust as needed
    to = as.Date("2021-12-01"),   # Adjust as needed
    by = "month"
  )
)

stringency_complete <- complete_dates %>%
  left_join(stringency_index, by = "formatted_date") %>%
  mutate(
    stringency_weighted_avg = ifelse(is.na(Stringency.index..weighted.average.), 0, Stringency.index..weighted.average.)
  )

library(dplyr)
library(lubridate)

# Ensure the 'Day' column is in date format
stringency_index <- stringency_index %>%
  mutate(Day = as.Date(Day))

# Group by month and calculate averages
stringency_monthly <- stringency_index %>%
  group_by(month = floor_date(Day, "month")) %>%
  summarise(
    avg_stringency_non_vaccinated = mean(Stringency.index..non.vaccinated., na.rm = TRUE),
    avg_stringency_vaccinated = mean(Stringency.index..vaccinated., na.rm = TRUE),
    avg_stringency_weighted_avg = mean(Stringency.index..weighted.average., na.rm = TRUE)
  )

stringency_monthly <- stringency_monthly %>%
  rename(formatted_date = month)

# Perform a left join to add stringency data to comb_data_date
comb_data_date <- comb_data_date %>%
  left_join(stringency_monthly, by = "formatted_date")

# Fill missing stringency values with 0, if applicable
comb_data_date <- comb_data_date %>%
  mutate(
    avg_stringency_non_vaccinated = ifelse(is.na(avg_stringency_non_vaccinated), 0, avg_stringency_non_vaccinated),
    avg_stringency_vaccinated = ifelse(is.na(avg_stringency_vaccinated), 0, avg_stringency_vaccinated),
    avg_stringency_weighted_avg = ifelse(is.na(avg_stringency_weighted_avg), 0, avg_stringency_weighted_avg)
  )

write_csv(comb_data_date, "comb_data_date.csv")

  # Load necessary libraries
  library(readr)
  library(lubridate)
  library(dplyr)
  
  # Load the datasets

  
  cum_covid <- read_csv("cumulative-confirmed-covid-19-cases-per-million-people.csv")
  week_covid <- read_csv("weekly-confirmed-covid-19-cases-per-million-people.csv")
  
  # Add 'year' and 'month' columns
  cum_covid <- cum_covid %>%
    mutate(
      year = year(Day),
      month = month(Day, label = TRUE, abbr = TRUE)  # Shortened month names
    )
  
  # Add 'year' and 'month' columns
  week_covid <- week_covid %>%
    mutate(
      year = year(Day),
      month = month(Day, label = TRUE, abbr = TRUE)  # Shortened month names
    )
  
  # Aggregate data to monthly level
  monthly_week_covid <- week_covid %>%
    group_by(Entity, year, month) %>%
    summarize(
      avg_weekly_cases = mean(`Weekly cases per million people`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Save the aggregated data to a CSV file
  write_csv(monthly_cum_covid, "monthly_cum_covid.csv")
  write_csv(monthly_week_covid, "monthly_week_covid.csv")
  
  
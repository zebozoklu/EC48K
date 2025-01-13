# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(TRmaps)
library(tidyr)


data("tr_nuts2")
tr_nuts2 <- st_as_sf(tr_nuts2)

cross_18 <- read_csv("cross_18.csv")
cross_19 <- read_csv("cross_19.csv")
cross_20 <- read_csv("cross_20.csv")
cross_21 <- read_csv("cross_21.csv")




# Define a function to calculate LFPR for a given dataset
calculate_lfpr <- function(cross_data, year) {
  lfpr_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FI010)) %>%  # Remove rows where HB031 or FI010 is NA
    group_by(HB031) %>%  # Group by NUTS2 region
    summarise(
      total_population = n(),  # Total population excluding NA in FI010
      labor_force_population = sum(FI010 %in% c(1, 2, 3, 4, 5), na.rm = TRUE),  # Labor force population
      labor_force_participation_rate = (labor_force_population / total_population) * 100  # LFPR
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(lfpr_data)
}

# Calculate LFPR for each cross-sectional dataset
lfpr_18 <- calculate_lfpr(cross_18, 2018)
lfpr_19 <- calculate_lfpr(cross_19, 2019)
lfpr_20 <- calculate_lfpr(cross_20, 2020)
lfpr_21 <- calculate_lfpr(cross_21, 2021)

# Combine all LFPR datasets into a single dataset
lfpr_all_years <- bind_rows(lfpr_18, lfpr_19, lfpr_20, lfpr_21)

# View the combined dataset
head(lfpr_all_years)

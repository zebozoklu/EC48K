# Define a function to calculate the rate of females in the labor force
calculate_female_labor_force_rate <- function(cross_data, year) {
  female_labor_force_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FI010), !is.na(FK090)) %>%  # Remove rows with NA in HB031, FI010, or FK090
    group_by(HB031) %>%  # Group by NUTS2 region
    summarise(
      labor_force_population = sum(FI010 %in% c(1, 2, 3, 4, 5), na.rm = TRUE),  # Total labor force population
      female_labor_force_population = sum(FK090 == 2 & FI010 %in% c(1, 2, 3, 4, 5), na.rm = TRUE),  # Females in labor force
      female_labor_force_rate = (female_labor_force_population / labor_force_population) * 100  # Rate of females in labor force
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(female_labor_force_data)
}

# Calculate the rate for each cross-sectional dataset
female_lf_18 <- calculate_female_labor_force_rate(cross_18, 2018)
female_lf_19 <- calculate_female_labor_force_rate(cross_19, 2019)
female_lf_20 <- calculate_female_labor_force_rate(cross_20, 2020)
female_lf_21 <- calculate_female_labor_force_rate(cross_21, 2021)

# Combine all datasets into a single dataset
female_lf_all_years <- bind_rows(female_lf_18, female_lf_19, female_lf_20, female_lf_21)

# View the combined dataset
head(female_lf_all_years)

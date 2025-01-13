# Define a function to calculate unemployment rate for a given dataset
calculate_unemployment_rate <- function(cross_data, year) {
  unemployment_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FI010)) %>%  # Remove rows with NA in HB031 or FI010
    group_by(HB031) %>%  # Group by NUTS2 region
    summarise(
      labor_force_population = sum(FI010 %in% c(1, 2, 3, 4, 5), na.rm = TRUE),  # Labor force population
      unemployed_population = sum(FI010 == 5, na.rm = TRUE),  # Unemployed population
      unemployment_rate = (unemployed_population / labor_force_population) * 100  # Unemployment rate
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(unemployment_data)
}

# Calculate unemployment rate for each cross-sectional dataset
unemployment_18 <- calculate_unemployment_rate(cross_18, 2018)
unemployment_19 <- calculate_unemployment_rate(cross_19, 2019)
unemployment_20 <- calculate_unemployment_rate(cross_20, 2020)
unemployment_21 <- calculate_unemployment_rate(cross_21, 2021)

# Combine all unemployment datasets into a single dataset
unemployment_all_years <- bind_rows(unemployment_18, unemployment_19, unemployment_20, unemployment_21)

# View the combined dataset
head(unemployment_all_years)

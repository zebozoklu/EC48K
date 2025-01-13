# Define a function to calculate the average education level for the labor force
calculate_education_rate_labor_force <- function(cross_data, year) {
  education_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FE030), FI010 %in% c(1, 2, 3, 4, 5)) %>%  # Filter for labor force
    group_by(HB031) %>%  # Group by NUTS2 region
    summarise(
      labor_force_population = n(),  # Total labor force population
      total_education_score = sum(FE030, na.rm = TRUE),  # Sum of education levels in the labor force
      average_education_level = total_education_score / labor_force_population  # Average education level
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(education_data)
}

# Calculate education rate for labor force for each cross-sectional dataset
education_lf_18 <- calculate_education_rate_labor_force(cross_18, 2018)
education_lf_19 <- calculate_education_rate_labor_force(cross_19, 2019)
education_lf_20 <- calculate_education_rate_labor_force(cross_20, 2020)
education_lf_21 <- calculate_education_rate_labor_force(cross_21, 2021)

# Combine all education datasets into a single dataset
education_lf_all_years <- bind_rows(education_lf_18, education_lf_19, education_lf_20, education_lf_21)

# View the combined dataset
head(education_lf_all_years)

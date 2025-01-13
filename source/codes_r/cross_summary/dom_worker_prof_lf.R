# Define a function to calculate the dominant worker profile for each NUTS2 region
calculate_worker_profile <- function(cross_data, year) {
  # Recode FI130 into broad categories
  cross_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FI010), !is.na(FI130), FI010 %in% c(1, 2, 3, 4, 5)) %>%  # Filter for labor force
    mutate(
      FI130_broad = case_when(
        FI130 %in% c(1, 2, 3) ~ "white_collar_hs",
        FI130 %in% c(4, 5) ~ "white_collar_ls",
        FI130 %in% c(6, 7) ~ "blue_collar_hs",
        FI130 %in% c(8, 9) ~ "blue_collar_ls",
        TRUE ~ NA_character_  # Handle unexpected cases
      )
    )
  
  # Calculate rates for white-collar and blue-collar workers
  worker_data <- cross_data %>%
    group_by(HB031) %>%
    summarise(
      labor_force_population = n(),
      white_collar_hs_rate = sum(FI130_broad == "white_collar_hs", na.rm = TRUE) / labor_force_population * 100,
      white_collar_ls_rate = sum(FI130_broad == "white_collar_ls", na.rm = TRUE) / labor_force_population * 100,
      blue_collar_hs_rate = sum(FI130_broad == "blue_collar_hs", na.rm = TRUE) / labor_force_population * 100,
      blue_collar_ls_rate = sum(FI130_broad == "blue_collar_ls", na.rm = TRUE) / labor_force_population * 100
    ) %>%
    mutate(
      white_collar_rate = white_collar_hs_rate + white_collar_ls_rate,  # Combined white-collar rate
      blue_collar_rate = blue_collar_hs_rate + blue_collar_ls_rate,    # Combined blue-collar rate
      dominant_profile = case_when(
        white_collar_rate > blue_collar_rate ~ "White Collar",
        blue_collar_rate > white_collar_rate ~ "Blue Collar",
        TRUE ~ "Equal"
      )
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(worker_data)
}

# Calculate worker profiles for each cross-sectional dataset
worker_profile_18 <- calculate_worker_profile(cross_18, 2018)
worker_profile_19 <- calculate_worker_profile(cross_19, 2019)
worker_profile_20 <- calculate_worker_profile(cross_20, 2020)
worker_profile_21 <- calculate_worker_profile(cross_21, 2021)

# Combine all datasets into a single dataset
worker_profiles_all_years <- bind_rows(worker_profile_18, worker_profile_19, worker_profile_20, worker_profile_21)

# View the combined dataset
head(worker_profiles_all_years)

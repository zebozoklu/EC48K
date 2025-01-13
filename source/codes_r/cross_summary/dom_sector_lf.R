# Define a function to calculate the dominant sector for each NUTS2 region
calculate_dominant_sector <- function(cross_data, year) {
  # Recode FI140 into three-sector categories
  cross_data <- cross_data %>%
    filter(!is.na(HB031), !is.na(FI010), !is.na(FI140), FI010 %in% c(1, 2, 3, 4, 5)) %>%  # Filter for labor force
    mutate(
      sector = case_when(
        FI140 %in% c("01", "02") ~ "Primary",
        FI140 %in% c("03", "04", "05") ~ "Secondary",
        FI140 %in% c("06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18") ~ "Tertiary",
        TRUE ~ NA_character_
      )
    )
  
  # Calculate rates for each sector
  sector_data <- cross_data %>%
    group_by(HB031) %>%
    summarise(
      labor_force_population = n(),
      primary_rate = sum(sector == "Primary", na.rm = TRUE) / labor_force_population * 100,
      secondary_rate = sum(sector == "Secondary", na.rm = TRUE) / labor_force_population * 100,
      tertiary_rate = sum(sector == "Tertiary", na.rm = TRUE) / labor_force_population * 100
    ) %>%
    mutate(
      dominant_sector = case_when(
        primary_rate > secondary_rate & primary_rate > tertiary_rate ~ "Primary",
        secondary_rate > primary_rate & secondary_rate > tertiary_rate ~ "Secondary",
        tertiary_rate > primary_rate & tertiary_rate > secondary_rate ~ "Tertiary",
        TRUE ~ "Equal"
      )
    ) %>%
    mutate(year = year)  # Add the year column for identification
  
  return(sector_data)
}

# Calculate dominant sectors for each cross-sectional dataset
sector_18 <- calculate_dominant_sector(cross_18, 2018)
sector_19 <- calculate_dominant_sector(cross_19, 2019)
sector_20 <- calculate_dominant_sector(cross_20, 2020)
sector_21 <- calculate_dominant_sector(cross_21, 2021)

# Combine all datasets into a single dataset
sectors_all_years <- bind_rows(sector_18, sector_19, sector_20, sector_21)

# View the combined dataset
head(sectors_all_years)

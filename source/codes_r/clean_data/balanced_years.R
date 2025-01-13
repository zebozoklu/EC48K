# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

silc18192021_p <- read_csv("GYKA-Panel/GYKA_Panel_2018-2021/silc18192021_p.csv")
silc18192021_pr <- read_csv("GYKA-Panel/GYKA_Panel_2018-2021/silc18192021_pr.csv")

panel_data_p <- silc18192021_p %>%
  select(HH_ID, PERS_ID, FB010, FB100, FE030, FI010, FI040, FI070, FI080, FI085, FI120, FI130, FI140, FI145, FI150,
         FI190, FI210, FI250, FI330, FI340A:FI340L, FG140)

merged_data <- panel_data_p %>%
  full_join(silc18192021_pr, by = c("HH_ID", "PERS_ID"))

balanced_panel <- merged_data %>%
  group_by(HH_ID, PERS_ID) %>%
  filter(n_distinct(FB010) == 4) %>%
  ungroup()

consistent_labor_force <- balanced_panel %>%
  pivot_longer(
    cols = starts_with("FI340"),  # Monthly activity status columns
    names_to = "month",
    values_to = "main_activity_status"
  ) %>%
  mutate(
    in_labor_force = ifelse(main_activity_status %in% c(1, 2, 3, 4, 5), 1, 0)  # Binary flag
  ) %>%
  group_by(HH_ID, PERS_ID) %>%  # Group only by individual (not year)
  summarize(stayed_in_labor_force = all(in_labor_force == 1)) %>%  # Stayed in labor force all 4 years
  filter(stayed_in_labor_force == TRUE)  # Only those consistently in labor force

balanced_panel <- balanced_panel %>%
  left_join(consistent_labor_force, by = c("HH_ID", "PERS_ID")) %>%
  filter(stayed_in_labor_force == TRUE)  # Keep only consistent participants

# Reshape and filter for labor force participants across all years
labor_force_data <- balanced_panel %>%
  pivot_longer(
    cols = starts_with("FI340"),  # Monthly activity status columns
    names_to = "month",
    values_to = "main_activity_status"
  ) %>%
  mutate(
    month = case_when(
      month == "FI340A" ~ "January",
      month == "FI340B" ~ "February",
      month == "FI340C" ~ "March",
      month == "FI340D" ~ "April",
      month == "FI340E" ~ "May",
      month == "FI340F" ~ "June",
      month == "FI340G" ~ "July",
      month == "FI340H" ~ "August",
      month == "FI340I" ~ "September",
      month == "FI340J" ~ "October",
      month == "FI340K" ~ "November",
      month == "FI340L" ~ "December"
    ),
    in_labor_force = ifelse(main_activity_status %in% c(1, 2, 3, 4, 5), 1, 0)
  ) %>%
  filter(in_labor_force == 1, stayed_in_labor_force == TRUE)


data_2018 <- labor_force_data %>% filter(FB010 == 2018, FK010 == 2018)
data_2019 <- labor_force_data %>% filter(FB010 == 2019, FK010 == 2019)
data_2020 <- labor_force_data %>% filter(FB010 == 2020, FK010 == 2020)
data_2021 <- labor_force_data %>% filter(FB010 == 2021, FK010 == 2021)


# Save as CSV files with the updated naming convention
write_csv(data_2018, "consistent_labor_force_2018.csv")
write_csv(data_2019, "consistent_labor_force_2019.csv")
write_csv(data_2020, "consistent_labor_force_2020.csv")
write_csv(data_2021, "consistent_labor_force_2021.csv")


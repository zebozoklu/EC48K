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

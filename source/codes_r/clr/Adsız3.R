# Join CLR data with absolute share data
sector_combined_data <- clr_data %>%
  select(formatted_date, clr_share_sector_Primary, clr_share_sector_Secondary, clr_share_sector_Tertiary) %>%
  left_join(comb_data_date %>% select(formatted_date, share_sector_Primary, share_sector_Secondary, share_sector_Tertiary), 
            by = "formatted_date") %>%
  pivot_longer(cols = starts_with("clr_share_sector"), names_to = "Sector", values_to = "CLR_Value") %>%
  mutate(Absolute_Share = case_when(
    Sector == "clr_share_sector_Primary" ~ share_sector_Primary,
    Sector == "clr_share_sector_Secondary" ~ share_sector_Secondary,
    Sector == "clr_share_sector_Tertiary" ~ share_sector_Tertiary
  )) %>%
  select(-share_sector_Primary, -share_sector_Secondary, -share_sector_Tertiary)

library(ggplot2)

sector_combined_data$formatted_date <- as.Date(sector_combined_data$formatted_date)


# Plot CLR and Absolute Shares
ggplot(sector_combined_data, aes(x = formatted_date, group = Sector)) +
  geom_line(aes(y = CLR_Value, color = Sector), linewidth = 1) +
  geom_line(aes(y = Absolute_Share, linetype = Sector), linewidth = 1, alpha = 0.7) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  labs(title = "Sectoral CLR and Absolute Shares in Unemployment with COVID Highlighted",
       x = "Date", y = "Value", color = "Sector (CLR)", linetype = "Sector (Absolute)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Absolute Shares")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

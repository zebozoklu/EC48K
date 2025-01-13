# Subset CLR and absolute share data for public and private sectors
public_private_data <- clr_data %>%
  select(formatted_date, clr_share_Public, clr_share_Private) %>%
  pivot_longer(cols = starts_with("clr_share_"), names_to = "Sector", values_to = "CLR_Value") %>%
  mutate(covid_period = ifelse(formatted_date >= as.Date("2020-03-01") & 
                                 formatted_date <= as.Date("2021-12-31"), "During COVID", "Pre-COVID"))


# Calculate mean CLR values for each sector and COVID period
public_private_summary <- public_private_data %>%
  group_by(Sector, covid_period) %>%
  summarise(mean_clr = mean(CLR_Value, na.rm = TRUE))

# Perform t-tests for public and private sectors
public_private_ttests <- public_private_data %>%
  group_by(Sector) %>%
  summarise(
    p_value = t.test(
      CLR_Value[covid_period == "Pre-COVID"],
      CLR_Value[covid_period == "During COVID"]
    )$p.value,
    mean_pre_covid = mean(CLR_Value[covid_period == "Pre-COVID"], na.rm = TRUE),
    mean_during_covid = mean(CLR_Value[covid_period == "During COVID"], na.rm = TRUE)
  )

public_private_ttests

public_private_data$formatted_date <- as.Date(public_private_data$formatted_date)


# Plot CLR trends for public and private sectors
ggplot(public_private_data, aes(x = formatted_date, group = Sector)) +
  geom_line(aes(y = CLR_Value, color = Sector), linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  labs(title = "Public and Private Sector CLR Trends with COVID Highlighted",
       x = "Date", y = "CLR Value", color = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

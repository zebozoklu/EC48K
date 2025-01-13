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

# Load the IBBS2 monthly data
ibbs2_monthly <- read_excel("ibbs2_monthly_per100k.xlsx")

# Convert month_year to Date type and extract year
ibbs2_monthly <- ibbs2_monthly %>%
  mutate(month_year = as.Date(month_year, format = "%Y-%m-%d"),
         year = year(month_year))  # Extract the year from the month_year column

# Filter data for February 2021 to February 2022
filtered_data <- ibbs2_monthly %>%
  filter(month_year >= as.Date("2021-02-01") & month_year <= as.Date("2022-02-28"))

# Calculate the average COVID rates per 100k for each region across this period
yearly_rates <- filtered_data %>%
  summarise(across(starts_with("TR"), ~ mean(. , na.rm = TRUE), .names = "{.col}")) %>%
  mutate(year = 2021)  # Add a "year" column for clarity

# Reshape the COVID data to long format
yearly_rates_long <- yearly_rates %>%
  pivot_longer(cols = starts_with("TR"), 
               names_to = "Region", 
               values_to = "COVID_Rate")

# View the reshaped data
head(yearly_rates_long)

# Merge the reshaped COVID data with the spatial data based on the region code
merged_data <- left_join(tr_nuts2, yearly_rates_long, by = c("NUTS2_code" = "Region"))

# Inspect the merged data
head(merged_data)

covid_plot_2021 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = COVID_Rate), color = "black") +  # Use the COVID rate column for coloring
  scale_fill_gradientn(colours = c("blue", "yellow", "red"), guide = "none") +  # Color scale
  geom_sf_text(aes(label = round(COVID_Rate, 1)), size = 3, color = "black", fontface = "bold") +  # Add labels with rounded COVID rates
  theme_void() +  # Remove axes and background
  ggtitle("COVID Rates by NUTS2 Region in Turkey - 2021") +  # Title for 2021
  labs(
    subtitle = "COVID rates per 100,000 people across NUTS2 regions in Turkey (2021)",
    caption = "Colors represent COVID rates per 100k: Blue (low), Yellow (medium), Red (high)\nData represents average COVID rates from February 2021 to February 2022"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    text = element_text(face = "bold")
  )

# Show the plot
covid_plot_2021


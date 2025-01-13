# Load necessary libraries
library(ggplot2)
library(sf)
library(TRmaps)
library(patchwork)  # For combining multiple plots

# Load Turkey's NUTS2 spatial data
data("tr_nuts2")
tr_nuts2 <- st_as_sf(tr_nuts2)

# Merge the unemployment rate data with the spatial data
map_data_unemployment <- left_join(tr_nuts2, unemployment_all_years, by = c("NUTS2_code" = "HB031"))

# Function to create a map for a specific year
create_unemployment_map <- function(year) {
  ggplot(data = filter(map_data_unemployment, year == !!year)) +
    geom_sf(aes(fill = unemployment_rate), color = "black") +
    scale_fill_gradientn(
      colors = c("green", "yellow", "red"),
      name = "Unemployment Rate (%)",
      limits = c(0, 40),  # Assuming unemployment rates are between 0-40%
      na.value = "gray"
    ) +
    theme_void() +
    ggtitle(paste("Unemployment Rate (", year, ")", sep = "")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom"
    )
}

# Create maps for each year
map_2018 <- create_unemployment_map(2018)
map_2019 <- create_unemployment_map(2019)
map_2020 <- create_unemployment_map(2020)
map_2021 <- create_unemployment_map(2021)

# Combine the maps into a single layout
combined_unemployment_maps <- (map_2018 | map_2019) / (map_2020 | map_2021)

# Save the combined map as a PNG file
ggsave("unemployment_maps_2018_2021.png", plot = combined_unemployment_maps, width = 12, height = 8)

# Display the combined maps
combined_unemployment_maps

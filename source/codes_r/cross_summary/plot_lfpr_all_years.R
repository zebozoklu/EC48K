# Load necessary libraries
library(ggplot2)
library(sf)
library(TRmaps)
library(patchwork)  # For combining multiple plots

# Load Turkey's NUTS2 spatial data
data("tr_nuts2")
tr_nuts2 <- st_as_sf(tr_nuts2)

# Merge the LFPR data with the spatial data
map_data_lfpr <- left_join(tr_nuts2, lfpr_all_years, by = c("NUTS2_code" = "HB031"))

# Function to create a map for a specific year
create_map <- function(year) {
  ggplot(data = filter(map_data_lfpr, year == !!year)) +
    geom_sf(aes(fill = labor_force_participation_rate), color = "black") +
    scale_fill_gradientn(
      colors = c("red", "yellow", "green"),
      name = "LFPR (%)",
      limits = c(20, 80),  # Assuming LFPR is within 0-100%
      na.value = "gray"
    ) +
    theme_void() +
    ggtitle(paste("Labor Force Participation Rate (", year, ")", sep = "")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom"
    )
}

# Create maps for each year
map_2018 <- create_map(2018)
map_2019 <- create_map(2019)
map_2020 <- create_map(2020)
map_2021 <- create_map(2021)

# Combine the maps into a single layout
combined_maps <- (map_2018 | map_2019) / (map_2020 | map_2021)

# Save the combined map as a PNG file
ggsave("lfpr_maps_2018_2021.png", plot = combined_maps, width = 12, height = 8)

# Display the combined maps
combined_maps

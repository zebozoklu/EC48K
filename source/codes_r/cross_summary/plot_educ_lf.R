# Load necessary libraries
library(ggplot2)
library(sf)
library(TRmaps)
library(patchwork)  # For combining multiple plots

# Load Turkey's NUTS2 spatial data
data("tr_nuts2")
tr_nuts2 <- st_as_sf(tr_nuts2)

# Merge the education level data with spatial data
map_data_education_lf <- left_join(tr_nuts2, education_lf_all_years, by = c("NUTS2_code" = "HB031"))

# Function to create a map for a specific year
create_education_lf_map <- function(year) {
  ggplot(data = filter(map_data_education_lf, year == !!year)) +
    geom_sf(aes(fill = average_education_level), color = "black") +
    scale_fill_gradientn(
      colors = c("red", "yellow", "green"),
      name = "Avg Education Level",
      limits = c(2, 8),  # Assuming the average levels range between 0 and 10
      na.value = "gray"
    ) +
    theme_void() +
    ggtitle(paste("Avg Education Level of Labor Force (", year, ")", sep = "")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom"
    )
}

# Create maps for each year
map_2018 <- create_education_lf_map(2018)
map_2019 <- create_education_lf_map(2019)
map_2020 <- create_education_lf_map(2020)
map_2021 <- create_education_lf_map(2021)

# Combine the maps into a single layout
combined_education_lf_maps <- (map_2018 | map_2019) / (map_2020 | map_2021)

# Save the combined map as a PNG file
ggsave("education_lf_maps_2018_2021.png", plot = combined_education_lf_maps, width = 12, height = 8)

# Display the combined maps
combined_education_lf_maps

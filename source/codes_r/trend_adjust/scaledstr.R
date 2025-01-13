library(forecast)

# Convert to time series
unemployment_ts <- ts(comb_data_date$unemployment_rate, start = c(2018, 1), frequency = 12)

# Decompose the time series
decomposition <- stl(unemployment_ts, s.window = "periodic")

# Plot decomposition
plot(decomposition)

# Save the plot as a PNG file
png("stl_decomposition_plot.png", width = 800, height = 600)
plot(decomposition)
dev.off()


scaledstr_ts <- ts(comb_data_date$scaled_stringency, start = c(2018,1), frequency = 12)

decomposition_str <- stl(scaledstr_ts, s.window = "periodic")

plot(decomposition_str)

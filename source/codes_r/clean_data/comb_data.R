pre_covid <- read_csv("pre_covid_unemployment.csv")
covid_era <- read_csv("covid_era_unemployment_with_covid_data.csv")

# Identify columns unique to the COVID-era data
missing_columns <- setdiff(colnames(covid_era), colnames(pre_covid))

# Add the missing columns to the pre-COVID dataset with default value 0
for (col in missing_columns) {
  pre_covid[[col]] <- 0
}

# Bind the two datasets together
combined_data <- rbind(pre_covid, covid_era)


# Install and load the compositions package
install.packages("compositions")
library(compositions)

# Define the columns to apply CLR transformation
share_columns <- c(
  "share_age_25.34", "share_age_35.44", "share_age_45.54", "share_age_55.",
  "share_age_.25", "share_gender_female", "share_gender_male",
  "share_education_high_school", "share_education_illiterate",
  "share_education_literate_no_school", "share_education_middle",
  "share_education_primary", "share_education_university_plus",
  "share_education_vocational", "share_marital_divorced",
  "share_marital_married", "share_marital_never_married",
  "share_marital_separated", "share_marital_widowed",
  "share_sector_Primary", "share_sector_Secondary",
  "share_sector_Tertiary", "share_blue_collar_hs",
  "share_blue_collar_ls", "share_white_collar_hs",
  "share_white_collar_ls", "share_employer", "share_paid_employment",
  "share_self_employed", "share_unpaid_family_worker",
  "share_wage_earner", "share_Private", "share_Public"
)

# Subset data for gender-related columns
gender_data <- comb_data_date[, c("share_gender_female", "share_gender_male")]

# Normalize proportions if necessary
gender_data <- gender_data / rowSums(gender_data)


# Apply CLR transformation
clr_gender <- as.data.frame(clr(gender_data))

# Rename CLR-transformed columns for clarity
colnames(clr_gender) <- c("clr_gender_female", "clr_gender_male")

# Create an empty dataset with the date column for alignment
clr_group <- comb_data_date["formatted_date"]

# Add CLR-transformed gender data to clr_group
clr_group <- cbind(clr_group, clr_gender)

# Subset data for age-related columns
age_data <- comb_data_date[, c("share_age_25.34", "share_age_35.44", 
                               "share_age_45.54", "share_age_55.", "share_age_.25")]

# Normalize proportions if needed
age_data <- age_data / rowSums(age_data)

# Apply CLR transformation
clr_age <- as.data.frame(clr(age_data))

# Rename CLR-transformed columns for clarity
colnames(clr_age) <- c("clr_age_25.34", "clr_age_35.44", 
                       "clr_age_45.54", "clr_age_55.", "clr_age_.25")

# Add CLR-transformed age data to clr_group
clr_group <- cbind(clr_group, clr_age)

# Subset data for education-related columns
education_data <- comb_data_date[, c("share_education_high_school", "share_education_illiterate", 
                                     "share_education_literate_no_school", "share_education_middle",
                                     "share_education_primary", "share_education_university_plus",
                                     "share_education_vocational")]

# Normalize proportions if needed
education_data <- education_data / rowSums(education_data)

# Apply CLR transformation
clr_education <- as.data.frame(clr(education_data))

# Rename CLR-transformed columns for clarity
colnames(clr_education) <- c("clr_education_high_school", "clr_education_illiterate", 
                             "clr_education_literate_no_school", "clr_education_middle",
                             "clr_education_primary", "clr_education_university_plus",
                             "clr_education_vocational")

# Add CLR-transformed education data to clr_group
clr_group <- cbind(clr_group, clr_education)

# Subset data for sector-related columns
sector_data <- comb_data_date[, c("share_sector_Primary", 
                                  "share_sector_Secondary", 
                                  "share_sector_Tertiary")]

# Normalize proportions if needed
sector_data <- sector_data / rowSums(sector_data)

# Apply CLR transformation
clr_sector <- as.data.frame(clr(sector_data))

# Rename CLR-transformed columns for clarity
colnames(clr_sector) <- c("clr_sector_Primary", 
                          "clr_sector_Secondary", 
                          "clr_sector_Tertiary")

# Add CLR-transformed sector data to clr_group
clr_group <- cbind(clr_group, clr_sector)

# Subset data for white-collar/blue-collar-related columns
collar_data <- comb_data_date[, c("share_blue_collar_hs", "share_blue_collar_ls", 
                                  "share_white_collar_hs", "share_white_collar_ls")]

# Normalize proportions if needed
collar_data <- collar_data / rowSums(collar_data)

# Apply CLR transformation
clr_collar <- as.data.frame(clr(collar_data))

# Rename CLR-transformed columns for clarity
colnames(clr_collar) <- c("clr_blue_collar_hs", "clr_blue_collar_ls", 
                          "clr_white_collar_hs", "clr_white_collar_ls")

# Add CLR-transformed collar data to clr_group
clr_group <- cbind(clr_group, clr_collar)

# Subset data for employment-type-related columns (including wage earner)
employment_data <- comb_data_date[, c("share_paid_employment", 
                                      "share_self_employed", 
                                      "share_wage_earner")]

# Normalize proportions if needed
employment_data <- employment_data / rowSums(employment_data)

# Apply CLR transformation
clr_employment <- as.data.frame(clr(employment_data))

# Rename CLR-transformed columns for clarity
colnames(clr_employment) <- c("clr_paid_employment", 
                              "clr_self_employed",
                              "clr_wage_earner")

# Add CLR-transformed employment data to clr_group
clr_group <- cbind(clr_group, clr_employment)

# Subset data for public/private-related columns
public_private_data <- comb_data_date[, c("share_Public", "share_Private")]

# Check if proportions sum to 1
row_sums_public_private <- rowSums(public_private_data)
all(row_sums_public_private == 1)  # Should return TRUE

# Normalize proportions if needed
public_private_data <- public_private_data / rowSums(public_private_data)

# Apply CLR transformation
clr_public_private <- as.data.frame(clr(public_private_data))

# Rename CLR-transformed columns for clarity
colnames(clr_public_private) <- c("clr_Public", "clr_Private")

# Add CLR-transformed public/private data to clr_group
clr_group <- cbind(clr_group, clr_public_private)


# Save clr_group to a CSV file
write.csv(clr_group, "clr_group.csv", row.names = FALSE)




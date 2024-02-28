# Read in species occurrences and background points using base R
species_occurrences <- read.csv("E:/GIS/Leucanthemopsis_Madrid/species_occurrences_adjusted_epsg2062.csv", stringsAsFactors = FALSE)
background_points <- read.csv("E:/GIS/Leucanthemopsis_Madrid/background_points_df_epsg2062.csv", stringsAsFactors = FALSE)

# Ensure column names and order match between species_occurrences and background_points
# Assuming background_points might not have the exact same columns or in the same order
background_points <- background_points[c("species", "X_Lon", "Y_Lat")]
background_points$species <- 0  # Ensuring this column is present and set to 0

# Find all unique species in the species occurrences file
unique_species <- unique(species_occurrences$species)

# Loop through each unique species
for (species_name in unique_species) {
  
  # Filter occurrences for the current species
  species_data <- subset(species_occurrences, species == species_name)
  
  # Change the species name in the species data to 1 for presence
  species_data$species <- 1
  
  # Ensure species_data has the same columns in the same order as background_points
  species_data <- species_data[c("species", "X_Lon", "Y_Lat")]
  
  # Combine species data with background points
  combined_data <- rbind(species_data, background_points)
  
  # Save combined data to CSV
  # Use species name directly as the filename, ensuring it matches exactly
  write.csv(combined_data, paste0("E:/GIS/Leucanthemopsis_Madrid/", species_name, ".csv"), row.names = FALSE, quote = FALSE)
}

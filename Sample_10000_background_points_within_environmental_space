#################
##background points
#################

# Load your environmental variables (assuming they are all in the same folder and have the same extent)
env_files <- list.files("E:/GIS/Leucanthemopsis_Madrid/tif", pattern="tif$", full.names=TRUE)
env_stack <- stack(env_files)

# Identify non-NA cells across the stack (i.e., land areas)
valid_cells <- !is.na(getValues(env_stack))

# Extract the coordinates of these non-NA cells
non_na_cells <- which(valid_cells)
background_points_xy <- xyFromCell(env_stack, non_na_cells)

# Convert to SpatialPoints
background_points <- SpatialPoints(background_points_xy, proj4string=CRS(proj4string(env_stack)))

# Extract the environmental data for the generated points
env_values <- extract(env_stack, background_points)

# Track which rows are not NA (keep these indices for later)
valid_rows <- !is.na(rowSums(env_values))
env_values_clean <- env_values[valid_rows, ]

# Perform PCA on the cleaned dataset
env_pca <- prcomp(env_values_clean, scale. = TRUE)

# Now, create SpatialPointsDataFrame only for valid (non-NA) points
# Extract coordinates for valid_rows only
valid_coords <- coordinates(background_points)[valid_rows, , drop = FALSE]

# Create SpatialPointsDataFrame with valid coordinates
background_points_df <- SpatialPointsDataFrame(coords = valid_coords, 
                                               data = data.frame(PC1 = env_pca$x[, 1], PC2 = env_pca$x[, 2]),
                                               proj4string = CRS(proj4string(env_stack)))

# And ensuring the row.names match between background_points_df and env_pca$x
background_points_df@data$PC1 <- env_pca$x[, 1]
background_points_df@data$PC2 <- env_pca$x[, 2]

# Assuming background_points_df is a SpatialPointsDataFrame with PC1 and PC2 scores
# Determine the number of bins for each principal component
num_bins <- sqrt(10000) # For example, 100 bins on each axis for ~10,000 combinations

# Create bins for each principal component
pc1_bins <- cut(background_points_df@data$PC1, breaks=num_bins)
pc2_bins <- cut(background_points_df@data$PC2, breaks=num_bins)

# Combine bins to form strata
background_points_df@data$stratum <- interaction(pc1_bins, pc2_bins)

# Sample points from each stratum
set.seed(123) # For reproducibility

# Pre-allocate a list to store sampled indices
sampled_indices <- list()

# Split the data into strata and sample one index per stratum
strata <- split(seq_len(nrow(background_points_df)), background_points_df@data$stratum)
for (stratum in strata) {
  if(length(stratum) > 0) {
    sampled_indices[[length(sampled_indices) + 1]] <- sample(stratum, size = 1)
  }
}

# Flatten the list of sampled indices
sampled_indices <- unlist(sampled_indices)

# Subset the original SpatialPointsDataFrame to keep only sampled points
sampled_points_df <- background_points_df[sampled_indices, ]

# Check if less than 10,000 points were sampled and adjust if necessary
final_points <- sampled_points_df
num_additional_points_needed <- 10000 - nrow(sampled_points_df)

if(num_additional_points_needed > 0) {
  # Sample additional points using base R's sample function
  all_indices <- seq_len(nrow(background_points_df))
  # Exclude already sampled indices
  available_indices <- setdiff(all_indices, sampled_indices)
  additional_indices <- sample(available_indices, size=num_additional_points_needed, replace=FALSE)
  
  # Subset the original SpatialPointsDataFrame to include the additional sampled points
  additional_points_df <- background_points_df[additional_indices, ]
  
  # Combine the sampled points with the additional points
  final_points <- rbind(sampled_points_df, additional_points_df)
}

# Set projection string for final_points (if not already set)
proj4string(final_points) <- CRS(proj4string(background_points_df))

# Remove PC1, PC2, and stratum columns from final_points
final_points@data <- final_points@data[, !(names(final_points@data) %in% c("PC1", "PC2", "stratum"))]

# Add a new column named "species" to final_points and set all values to 0
final_points@data$species <- 0

# Create a data frame for EPSG2062 export, with "species" as the first column
export_df_eps2062 <- data.frame(species = final_points@data$species,
                                X_Lon = coordinates(final_points)[, 1],
                                Y_Lat = coordinates(final_points)[, 2])

# Save in EPSG2062
write.csv(export_df_eps2062, "background_points_df_epsg2062.csv", row.names = FALSE)

# Transform final_points to WGS84 for saving
final_points_wgs84 <- spTransform(final_points, CRS("+proj=longlat +datum=WGS84"))

# Create a data frame for the WGS84 version, with "species" as the first column
export_df_wgs84 <- data.frame(species = final_points_wgs84@data$species,
                              X_Lon = coordinates(final_points_wgs84)[, 1],
                              Y_Lat = coordinates(final_points_wgs84)[, 2])

# Save the WGS84 version
write.csv(export_df_wgs84, "background_points_df_wgs84.csv", row.names = FALSE)


#first rarefy occurrences so that each raster cell will contain exactly one point
# Load necessary libraries
library(raster)
library(rgdal)
library(sp)

# Set working directory - adjust as necessary
setwd("E:/GIS/Leucanthemopsis_Madrid")

# Load the raster and set its CRS
bio <- raster("E:/GIS/Leucanthemopsis_Madrid/tif/CHELSA_bio2_1981.2010_V.2.tif")
#for that project I'm using EPSG:2062 projection
newproj <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +rf=294.73 +pm=-3.687375 +units=m +no_defs +type=crs"
crs(bio) <- newproj

# Load the occurrences data
pres <- read.csv("D:/iberian_LPS/Lps_all.csv", header=TRUE)

# Ensure coordinates are in the correct CRS
coordinates(pres) <- ~X_Long+Y_Lat
proj4string(pres) <- CRS("+proj=longlat +datum=WGS84")

# Transform to the same CRS as the raster
pres <- spTransform(pres, CRS(newproj))

# Get unique species names
species <- unique(pres$Spec)

# Loop through each species
for (spec in species) {
  # Subset data for the current species
  spec_data <- pres[pres$Spec == spec, ]
  
  # Create a temporary raster to hold the rasterized data for the current species
  ras_temp <- raster(nrows=nrow(bio), ncols=ncol(bio), ext=extent(bio), crs=crs(bio))
  
  # Rasterize occurrences for the current species
  ras_temp <- rasterize(spec_data, ras_temp, field=1, background=0, update=TRUE, updateValue='all', na.rm=TRUE)
  
  # Convert to points, keeping only cells with occurrences
  spec_points <- rasterToPoints(ras_temp, fun=function(x){x>=1}, spatial=TRUE)
  
  # Add species name to the points dataframe
  spec_points$species <- spec
  
  # If first species, initialize result dataframe, else bind
  if (spec == species[1]) {
    result_points <- spec_points
  } else {
    result_points <- rbind(result_points, spec_points)
  }
}

# Optionally, convert the SpatialPointsDataFrame back to a regular dataframe and write to CSV
result_df <- as.data.frame(result_points)

# Drop the first column (if it's always the 'layer' column as in the rasterToPoints output)
result_df <- result_df[, -1]

# Rename the coordinate columns to include "Lat" and "Lon"
names(result_df)[names(result_df) == "x"] <- "X_Lon"
names(result_df)[names(result_df) == "y"] <- "Y_Lat"

# Save the EPSG 2062 version
write.csv(result_df, "species_occurrences_adjusted_epsg2062.csv", row.names=FALSE)

# Transform the SpatialPointsDataFrame to WGS84
result_points_wgs84 <- spTransform(result_points, CRS("+proj=longlat +datum=WGS84"))

# Convert the WGS84 SpatialPointsDataFrame to a regular dataframe
result_df_wgs84 <- as.data.frame(result_points_wgs84)

# Drop the 'layer' column (assuming it's the first column as before)
result_df_wgs84 <- result_df_wgs84[, -1]

# Rename the coordinate columns to include "Lat" and "Lon"
names(result_df_wgs84)[names(result_df_wgs84) == "x"] <- "X_Lon"
names(result_df_wgs84)[names(result_df_wgs84) == "y"] <- "Y_Lat"

# Save the WGS84 version
write.csv(result_df_wgs84, "species_occurrences_adjusted__wgs84.csv", row.names=FALSE)

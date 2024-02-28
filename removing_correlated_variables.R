require(raster)
require(rgdal)
rasterOptions(tmpdir="F:/GIS_awaryjny", progress="text", timer=TRUE)
setwd("C:/GIS_roboczy/Lps_Madrid_Layers/all_layers_uncorrelated")
files <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/all_layers_uncorrelated",pattern='tif',full.names=TRUE)

#align rasters to a common extent
baseRaster <- raster("C:/GIS_roboczy/Lps_Madrid_Layers/all_layers_uncorrelated/CHELSA_bio1_1981.2010_V.2.tif")
newproj <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +rf=294.73 +pm=-3.687375 +units=m +no_defs +type=crs"
crs(baseRaster) <- newproj

# Determine a common extent and resolution - for this example, we use the extent and resolution of the first raster
# You might want to define this manually or use another method to determine the best extent and resolution for your case
commonExtent <- extent(baseRaster)
commonResolution <- res(baseRaster)

alignedRasters <- lapply(files, function(file) {
  r <- raster(file)
  
  # Check if CRS matches the baseRaster/newproj, reproject if not
  if (!identical(crs(r), newproj)) {
    r <- projectRaster(r, crs=newproj)
  }
  
  rResampled <- resample(r, baseRaster, method='bilinear')
  rCropped <- crop(rResampled, commonExtent)
  rAligned <- aggregate(rCropped, fact=commonResolution/res(rCropped), fun=mean)
  
  return(rAligned)
})

Grids <- raster::stack(alignedRasters)

#check if any of the rasters in the raster stack have an excess amount of missing data. 
# Create a data frame to hold layer names and their corresponding NA percentages
na_report <- data.frame(
  Layer = names(Grids),
  NA_Percentage = sapply(1:nlayers(Grids), function(i) {
    layer_values <- getValues(Grids[[i]])
    na_count <- sum(is.na(layer_values))
    total_count <- length(layer_values)
    na_percentage <- (na_count / total_count) * 100
    return(na_percentage)
  })
)

# Sort the report by NA_Percentage in descending order
na_report_sorted <- na_report[order(-na_report$NA_Percentage), ]

# View the sorted report
print(na_report_sorted)
#after identifying any layer with NA values close to 100 it should be removed. You can do this by making a liast of such layers as in the example below and then delete them from the raster stack
layers_to_remove <- c("heat_load_index_normalized_R", "heat_load_index_R", "CHELSA_gddlgd0_1981.2010_V.2", "CHELSA_gdgfgd0_1981.2010_V.2", "CHELSA_swe_1981.2010_V.2", "CHELSA_fcf_1981.2010_V.2", "CHELSA_gddlgd5_1981.2010_V.2", "CHELSA_gdgfgd5_1981.2010_V.2")
# Remove the specified layers from the raster stack
Grids_cleaned <- dropLayer(Grids, layers_to_remove)


###Corelation and deletion of colrelated variables
#this process first works on the original "Grids_cleaned" which is silence after the first step and the run on a reduced dataset "Grids_uncor"
#values_df <- as.data.frame(values(Grids_cleaned))
values_df <- as.data.frame(values(Grids_uncor))
complete_cases_df <- na.omit(values_df)
# Compute correlations on the cleaned data
corr_matrix <- cor(complete_cases_df, method="pearson")
# Convert the correlation matrix into a long-format data frame
corr_df <- as.data.frame(as.table(corr_matrix))
# Rename columns for clarity
names(corr_df) <- c("Variable1", "Variable2", "Correlation")
# Remove self-correlations
corr_df <- subset(corr_df, Variable1 != Variable2)
# Remove duplicate correlations (Var1-Var2 is the same as Var2-Var1)
corr_df <- corr_df[!duplicated(t(apply(corr_df[,1:2], 1, sort))),]
# Sort by absolute correlation value, descending
corr_df <- corr_df[order(-abs(corr_df$Correlation)),]
# View the top N correlations
head(corr_df, n = 15)

#now manually select the layers that you want to exclude and those that you want to keep. It has to be done manually since it is easier to decide which variable may be more important for the species. Though in fact if they are correlated to a high degree it means that basicially they represent the same phenomenon. 
#I always select few layers but only once. So if a layer1 is appearing several times in high correlation I'm dropping it once and compute correlations again - this is not to delete too much variables. It is also easier to decide if a process is done sequentially. 
#layers_corelated <- c("wind_cwe", "vapr_cwe", "srad_cwe", "local_relief_R", "CHELSA_gdd0_1981.2010_V.2", "CHELSA_sfcWind_max_1981.2010_V.2")
#layers_corelated <- c("CHELSA_gdd10_1981.2010_V.2", "CHELSA_gdgfgd10_1981.2010_V.2", "CHELSA_bio13_1981.2010_V.2", "Visible.Sky", "CHELSA_ngd0_1981.2010_V.2", "CHELSA_sfcWind_min_1981.2010_V.2")
#layers_corelated <- c("CHELSA_gdd5_1981.2010_V.2", "CHELSA_bio6_1981.2010_V.2", "wind_max", "CHELSA_vpd_range_1981.2010_V.2", "CHELSA_bio5_1981.2010_V.2", "tri_qgis", "CHELSA_bio14_1981.2010_V.2", "CHELSA_cmi_max_1981.2010_V.2")
#layers_corelated <- c("wind_min", "CHELSA_bio16_1981.2010_V.2", "CHELSA_vpd_mean_1981.2010_V.2", "CHELSA_ngd10_1981.2010_V.2", "roughness_R", "vapr_mean")
#layers_corelated <- c("CHELSA_bio18_1981.2010_V.2", "CHELSA_bio1_1981.2010_V.2", "CHELSA_cmi_mean_1981.2010_V.2", "srad_cv", "X1st_order_partial_derivative_N_S_slope_grassGIS_qgis")
#layers_corelated <- c("vapr_min", "CHELSA_pet_penman_max_1981.2010_V.2", "CHELSA_vpd_max_1981.2010_V.2", "CHELSA_bio7_1981.2010_V.2", "srad_min", "silt_igh_0_5")
#layers_corelated <- c("CHELSA_vpd_min_1981.2010_V.2", "CHELSA_hurs_min_1981.2010_V.2", "CHELSA_cmi_min_1981.2010_V.2", "CHELSA_bio12_1981.2010_V.2")
#layers_corelated <- c("CHELSA_ngd5_1981.2010_V.2", "soc_igh_0_5", "Valley.Depth", "Sky.View.Factor", "ocs_igh_0_5")
#layers_corelated <- c("vapr_max", "srad_max", "nitrogen_igh_0_5", "CHELSA_cmi_range_1981.2010_V.2", "Analytical.Hillshading", "CHELSA_npp_1981.2010_V.2", "clay_igh_0_5", "profile_curvature_qgis", "Channel.Network.Distance", "CHELSA_pet_penman_min_1981.2010_V.2")
#layers_corelated <- c("CHELSA_gst_1981.2010_V.2", "vapr_cv", "tangential_curvature_qgis", "CHELSA_bio17_1981.2010_V.2", "slope_R", "CHELSA_pet_penman_range_1981.2010_V.2")
#layers_corelated <- c("CHELSA_bio10_1981.2010_V.2")
layers_corelated <- c("wind_mean")
#Grids_uncor <- dropLayer(Grids_cleaned, layers_corelated)
Grids_uncor <- dropLayer(Grids_uncor, layers_corelated)
#repeat iuntil you get correlation below a definied value, let's say 0.80


#delete variables with zeo or near zero variance
# Assuming Grids_uncor is your RasterStack
variances <- sapply(1:nlayers(Grids_uncor), function(i) {
  layer_values <- getValues(Grids_uncor[[i]])
  var(na.omit(layer_values), na.rm = TRUE)
})
near_zero_threshold <- 0.01
layers_to_keep <- variances > near_zero_threshold

Grids_uncor <- subset(Grids_uncor, which(layers_to_keep))


layer_names <- names(Grids_uncor)
print(layer_names)

setwd("E:/GIS/Leucanthemopsis_Madrid")
#writeRaster(Grids_uncor, filename=names_Grids_uncor, format="ascii", prj=TRUE, bylayer=TRUE, suffix=layer_names, overwrite=TRUE)
#writeRaster(Grids_uncor, filename=names_Grids_uncor, format="GTiff", bylayer=TRUE, suffix=layer_names, overwrite=TRUE)

#alternative approach to saving using the names
# Define a target resolution that you want for your raster
# It should be square, meaning the x and y resolutions are the same.
# Choose a value based on your specific needs and the original data resolution
target_resolution <- min(res(Grids_uncor))

# Iterate over each layer in the RasterStack
for (i in 1:length(layer_names)) {
  # Access the individual layer
  layer <- Grids_uncor[[i]]
  
  # Create a template raster with the desired (square) resolution
  template <- raster(extent(layer), nrows=nrow(layer), ncols=ncol(layer), 
                     xmn=extent(layer)[1], xmx=extent(layer)[2], 
                     ymn=extent(layer)[3], ymx=extent(layer)[4], 
                     crs=proj4string(layer), res=target_resolution)
  
  # Resample the layer to have square cells using the template
  resampled_layer <- resample(layer, template, method="bilinear")
  
  # Define the filename for this resampled layer, using the layer name
  filename_ascii <- paste0("E:/GIS/Leucanthemopsis_Madrid/", layer_names[i], ".asc")
  
  # Write the resampled layer as an ASCII file
  writeRaster(resampled_layer, filename=filename_ascii, format="ascii", overwrite=TRUE)
}

# Similarly for GeoTIFF
for (i in 1:length(layer_names)) {
  filename_tiff <- paste0(layer_names[i], ".tif")
  
  # Write the current layer as a GeoTIFF file
  writeRaster(Grids_uncor[[i]], filename=filename_tiff, format="GTiff", overwrite=TRUE)
}



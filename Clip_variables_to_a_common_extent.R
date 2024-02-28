require(raster)
require(rgdal)
rasterOptions(tmpdir="F:/GIS_awaryjny_TEMP2", progress="text", timer=TRUE)
#cutting raster
#raster00 <- raster("E:/Future/2070/ascii_rcp26/climate_climate_CHELSA_bio_1.asc") 
#raster00 <- raster("M:/GIS/ROTUNDIFOLIUM/Carpathians_ascii_30sec/CHELSA_ENVIREM_for_maxent_bilinear/chelsa_car_sil_CHELSA_bio_11.asc") 
#cutting vector
#maska <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="convex_hull_100km_WGS84")
maska <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="iberian_penin_WGS84")

#MASK WORLDCLIM

#dir.create("E:/nowy_folder")
setwd("C:/GIS_roboczy/Lps_Madrid_Layers")
files_worldclim <- list.files("E:/GIS/CHELSA/CHELSA_2.0/1981-2010/bio",pattern='tif',full.names=TRUE)
other_files <- list.files("E:/GIS/CHELSA/CHELSA_2.0/1981-2010/bio",pattern='aux',full.names=TRUE)
files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names_worldclim <- names(grids_worldclim)

newproj <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +rf=294.73 +pm=-3.687375 +units=m +no_defs +type=crs"

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska)
#soil_mask <- mask(grids_worldclim, maska)
soil_mask <- mask(soil_crop, maska)
#soil_rsmpl <- resample(soil_crop, raster00, method="ngb")
soil_reprojected <- projectRaster(soil_mask, crs=newproj)
#writeRaster(soil_mask, filename="masked.asc", format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
#writeRaster(soil_mask, filename=names_worldclim, format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
#writeRaster(soil_mask, filename=names_worldclim, format="GTiff", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
writeRaster(soil_reprojected, filename=names_worldclim, format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
writeRaster(soil_reprojected, filename=names_worldclim, format="GTiff", bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)


##SOIL
raster00 <- raster("C:/GIS_roboczy/Lps_Madrid_Layers/chelsa_iberian/CHELSA_bio1_1981.2010_V.2.tif") 
maska2 <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="iberian_penin_EPSG2062")
files_worldclim <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/soil",pattern='tif',full.names=TRUE)
other_files <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/soil",pattern='aux',full.names=TRUE)
files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
names_worldclim <- names(grids_worldclim)

newproj <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +rf=294.73 +pm=-3.687375 +units=m +no_defs +type=crs"

soil_reprojected <- projectRaster(grids_worldclim, crs=newproj)
soil_rsmpl <- resample(soil_reprojected, raster00, method="bilinear")
soil_crop <- crop(soil_rsmpl, maska2)
soil_mask <- mask(soil_rsmpl, maska2)

writeRaster(soil_mask, filename=names_worldclim, format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
writeRaster(soil_mask, filename=names_worldclim, format="GTiff", bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)

##WIND
files_worldclim <- list.files("E:/GIS/worldclim2/wc2.1_30s_wind",pattern='tif',full.names=TRUE)
#other_files <- list.files("E:/GIS/worldclim2/wc2.1_30s_wind",pattern='aux',full.names=TRUE)
#files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names_worldclim <- names(grids_worldclim)

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska)
#soil_mask <- mask(grids_worldclim, maska)
soil_mask <- mask(soil_crop, maska)
soil_reprojected <- projectRaster(soil_mask, crs=newproj)
soil_rsmpl <- resample(soil_reprojected, raster00, method="bilinear")
wind_max <- calc(soil_rsmpl, max, na.rm = TRUE)
wind_min <- calc(soil_rsmpl, min, na.rm = TRUE)
wind_mean <- calc(soil_rsmpl, mean, na.rm = TRUE)
wind_sd <- calc(soil_rsmpl, sd, na.rm = TRUE)
wind_cv <- wind_sd / wind_mean
#Cumulative Wind Exposure
wind_cwe <- sum(soil_rsmpl)

writeRaster(wind_max, filename="wind_max.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_max, filename="wind_max.tif", format="GTiff", overwrite=TRUE)
writeRaster(wind_min, filename="wind_min.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_min, filename="wind_min.tif", format="GTiff", overwrite=TRUE)
writeRaster(wind_mean, filename="wind_mean.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_mean, filename="wind_mean.tif", format="GTiff", overwrite=TRUE)
writeRaster(wind_sd, filename="wind_sd.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_sd, filename="wind_sd.tif", format="GTiff", overwrite=TRUE)
writeRaster(wind_cv, filename="wind_cv.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_cv, filename="wind_cv.tif", format="GTiff", overwrite=TRUE)
writeRaster(wind_cwe, filename="wind_cwe.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(wind_cwe, filename="wind_cwe.tif", format="GTiff", overwrite=TRUE)

##srad
files_worldclim <- list.files("E:/GIS/worldclim2/wc2.1_30s_srad",pattern='tif',full.names=TRUE)
#other_files <- list.files("E:/GIS/worldclim2/wc2.1_30s_srad",pattern='aux',full.names=TRUE)
#files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names_worldclim <- names(grids_worldclim)

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska)
#soil_mask <- mask(grids_worldclim, maska)
soil_mask <- mask(soil_crop, maska)
soil_reprojected <- projectRaster(soil_mask, crs=newproj)
soil_rsmpl <- resample(soil_reprojected, raster00, method="bilinear")
srad_max <- calc(soil_rsmpl, max, na.rm = TRUE)
srad_min <- calc(soil_rsmpl, min, na.rm = TRUE)
srad_mean <- calc(soil_rsmpl, mean, na.rm = TRUE)
srad_sd <- calc(soil_rsmpl, sd, na.rm = TRUE)
srad_cv <- srad_sd / srad_mean
#Cumulative srad Exposure
srad_cwe <- sum(soil_rsmpl)

writeRaster(srad_max, filename="srad_max.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_max, filename="srad_max.tif", format="GTiff", overwrite=TRUE)
writeRaster(srad_min, filename="srad_min.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_min, filename="srad_min.tif", format="GTiff", overwrite=TRUE)
writeRaster(srad_mean, filename="srad_mean.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_mean, filename="srad_mean.tif", format="GTiff", overwrite=TRUE)
writeRaster(srad_sd, filename="srad_sd.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_sd, filename="srad_sd.tif", format="GTiff", overwrite=TRUE)
writeRaster(srad_cv, filename="srad_cv.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_cv, filename="srad_cv.tif", format="GTiff", overwrite=TRUE)
writeRaster(srad_cwe, filename="srad_cwe.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(srad_cwe, filename="srad_cwe.tif", format="GTiff", overwrite=TRUE)


##vapr
files_worldclim <- list.files("E:/GIS/worldclim2/wc2.1_30s_vapr",pattern='tif',full.names=TRUE)
#other_files <- list.files("E:/GIS/worldclim2/wc2.1_30s_vapr",pattern='aux',full.names=TRUE)
#files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names_worldclim <- names(grids_worldclim)

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska)
#soil_mask <- mask(grids_worldclim, maska)
soil_mask <- mask(soil_crop, maska)
soil_reprojected <- projectRaster(soil_mask, crs=newproj)
soil_rsmpl <- resample(soil_reprojected, raster00, method="bilinear")
vapr_max <- calc(soil_rsmpl, max, na.rm = TRUE)
vapr_min <- calc(soil_rsmpl, min, na.rm = TRUE)
vapr_mean <- calc(soil_rsmpl, mean, na.rm = TRUE)
vapr_sd <- calc(soil_rsmpl, sd, na.rm = TRUE)
vapr_cv <- vapr_sd / vapr_mean
#Cumulative vapr Exposure
vapr_cwe <- sum(soil_rsmpl)

writeRaster(vapr_max, filename="vapr_max.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_max, filename="vapr_max.tif", format="GTiff", overwrite=TRUE)
writeRaster(vapr_min, filename="vapr_min.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_min, filename="vapr_min.tif", format="GTiff", overwrite=TRUE)
writeRaster(vapr_mean, filename="vapr_mean.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_mean, filename="vapr_mean.tif", format="GTiff", overwrite=TRUE)
writeRaster(vapr_sd, filename="vapr_sd.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_sd, filename="vapr_sd.tif", format="GTiff", overwrite=TRUE)
writeRaster(vapr_cv, filename="vapr_cv.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_cv, filename="vapr_cv.tif", format="GTiff", overwrite=TRUE)
writeRaster(vapr_cwe, filename="vapr_cwe.asc", format="ascii", prj=TRUE, overwrite=TRUE)
writeRaster(vapr_cwe, filename="vapr_cwe.tif", format="GTiff", overwrite=TRUE)


###Terrain
elevation <- raster("E:/GIS/worldclim2/wc2.1_30s_elev/wc2.1_30s_elev.tif")
crs(elevation) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
elevation_crop <- crop(elevation, maska)
elevation_mask <- mask(elevation_crop, maska)
elevation_reprojected <- projectRaster(elevation_mask, crs=newproj)
elevation_rsmpl <- resample(elevation_reprojected, raster00, method="bilinear")

slope <- terrain(elevation_rsmpl, opt = 'slope', unit = 'degrees')
aspect <- terrain(elevation_rsmpl, opt = 'aspect', unit = 'degrees') # Aspect in degrees (0-360)

roughness <- terrain(elevation_rsmpl, opt = 'roughness')

#Elevation Range within a Local Area (Local Relief)
# Define a matrix that specifies the neighborhood size, e.g., a 3x3 cell window
window <- matrix(1, nrow=3, ncol=3)

# Calculate local relief
local_relief <- focal(elevation_rsmpl, w=window, fun=function(x) max(x, na.rm=TRUE) - min(x, na.rm=TRUE))

#Heat Load Index, HLI, slope in radians for HLI calculation
slope_radians <- terrain(elevation_rsmpl, opt = 'slope', unit = 'radians')
hli <- cos(aspect) * sin(slope_radians) + cos(slope_radians) * (1 - abs(sin(aspect)))
hli_normalized = (hli - min(hli)) / (max(hli) - min(hli))

writeRaster(slope, filename="slope_R.tif", format="GTiff", overwrite=TRUE)
writeRaster(aspect, filename="aspect_R.tif", format="GTiff", overwrite=TRUE)
writeRaster(roughness, filename="roughness_R.tif", format="GTiff", overwrite=TRUE)
writeRaster(local_relief, filename="local_relief_R.tif", format="GTiff", overwrite=TRUE)
writeRaster(hli_normalized, filename="heat_load_index_normalized_R.tif", format="GTiff", overwrite=TRUE)
writeRaster(hli, filename="heat_load_index_not_normalized_R.tif", format="GTiff", overwrite=TRUE)

##SAGA
files_worldclim <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/saga",pattern='tif',full.names=TRUE)
#other_files <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/saga",pattern='aux',full.names=TRUE)
#files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- newproj
names_worldclim <- names(grids_worldclim)

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska2)
soil_mask <- mask(soil_crop, maska2)
soil_rsmpl <- resample(soil_mask, raster00, method="bilinear")

writeRaster(soil_rsmpl, filename=names_worldclim, format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
writeRaster(soil_rsmpl, filename=names_worldclim, format="GTiff", bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)


##QGIS
files_worldclim <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/qgis",pattern='tif',full.names=TRUE)
other_files <- list.files("C:/GIS_roboczy/Lps_Madrid_Layers/qgis",pattern='aux',full.names=TRUE)
files_worldclim <- files_worldclim[!files_worldclim %in% other_files]
grids_worldclim <- raster::stack(files_worldclim)
crs(grids_worldclim) <- newproj
names_worldclim <- names(grids_worldclim)

#"crop" crops by extent only, to crop more specific areas use "mask"
soil_crop <- crop(grids_worldclim, maska2)
soil_mask <- mask(soil_crop, maska2)
soil_rsmpl <- resample(soil_mask, raster00, method="bilinear")

writeRaster(soil_rsmpl, filename=names_worldclim, format="ascii", prj=TRUE, bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)
writeRaster(soil_rsmpl, filename=names_worldclim, format="GTiff", bylayer=TRUE, suffix=names_worldclim, overwrite=TRUE)


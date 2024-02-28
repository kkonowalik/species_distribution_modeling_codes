#For this code I initially used and started with:
#Lizarazo, I. 2023. How to download soil data from SoilGrids. Available at: https://rpubs.com/ials2un/gettingSoilGrids

#Code fol all variables

library(XML)
library(rgdal)
library(gdalUtils)
library(raster)
library(stars)
library(sf)
library(dplyr)
library(RColorBrewer)
library(mapview)
library(tmap)

# Assuming the GDAL path and environment variable setup is done before this

setwd("C:/GIS_roboczy/Lps_Madrid_Layers/soil")

url = "https://files.isric.org/soilgrids/latest/data/"

# Variables to loop through
variables = c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb")
depth = "0-5cm"
quantile = "mean"

# Load shapefile for bounding box
stder = st_read("D:/Salvatore_iberian_LPS/locations/iberian_penin_WGS84.shp")
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
stder_igh <- st_transform(stder, igh)
bbox <- st_bbox(stder_igh)
ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
bb <- c(ulx, uly, lrx, lry)

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"

for (voi in variables) {
  # Construct the file and URL names
  vrt_layer = paste(url, voi, depth, quantile, sep="_")
  vrt_file = paste(vrt_layer, '.vrt', sep="")
  data_url = paste(sg_url, voi, "/", voi, "_", depth, "_", quantile, ".vrt", sep="")
  output_file = paste(voi, "_igh_0_5.tif", sep="")
  
  # Download and save the data
  gdal_translate(data_url, output_file,
                 tr=c(250,250),
                 projwin=bb,
                 projwin_srs=igh,
                 verbose=TRUE)
}







#########FOR single variable
#this is template for soc
#belowe it there are options to download missing vars, some have for example different depth not 0-5 cm but 0-30 cm


library(XML)
library(rgdal)
library(gdalUtils)
library(raster)
library(stars)
library(sf)
library(dplyr)
library(RColorBrewer)
library(mapview)
library(tmap)

# Define the path to the GDAL binaries
gdal_path <- "C:/Program Files/QGIS 3.32.3/bin"

# Add the GDAL path to the beginning of the current PATH environment variable
# This ensures that the specified GDAL path is searched first
Sys.setenv(PATH = paste(gdal_path, Sys.getenv("PATH"), sep=";"))

# Now you can run your gdal_translate or other GDAL-related commands


setwd("C:/GIS_roboczy/Lps_Madrid_Layers/soil")

url = "https://files.isric.org/soilgrids/latest/data/" 

# basic strings
voi = "soc" 
depth = "0-5cm"
quantile = "mean"
# concatenate the strings
(variable = paste(url, voi, sep=""))

(layer = paste(variable,depth,quantile, sep="_"))

(vrt_layer = paste(layer, '.vrt', sep=""))

#maska <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="iberian_penin_WGS84")

stder = st_read("D:/Salvatore_iberian_LPS/locations/iberian_penin_WGS84.shp")

igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
stder_igh <- st_transform(stder, igh)

bbox <- st_bbox(stder_igh)

ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
datos = 'soc/soc_0-5cm_mean.vrt'
file = "soc_igh_0_5.tif"

gdal_translate(paste0(sg_url,datos), file ,
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh,
               verbose=TRUE)


#######################################
#######################################
#######################################


# basic strings
voi = "phh2o" 
depth = "0-5cm"
quantile = "mean"
# concatenate the strings
(variable = paste(url, voi, sep=""))

(layer = paste(variable,depth,quantile, sep="_"))

(vrt_layer = paste(layer, '.vrt', sep=""))

#maska <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="iberian_penin_WGS84")

stder = st_read("D:/Salvatore_iberian_LPS/locations/iberian_penin_WGS84.shp")

igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
stder_igh <- st_transform(stder, igh)

bbox <- st_bbox(stder_igh)

ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
datos = 'phh2o/phh2o_0-5cm_mean.vrt'
file = "phh2o_igh_0_5.tif"

gdal_translate(paste0(sg_url,datos), file ,
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh,
               verbose=TRUE)




# basic strings
voi = "ocs" 
depth = "0-30cm"
quantile = "mean"
# concatenate the strings
(variable = paste(url, voi, sep=""))

(layer = paste(variable,depth,quantile, sep="_"))

(vrt_layer = paste(layer, '.vrt', sep=""))

#maska <- readOGR(dsn="D:/Salvatore_iberian_LPS/locations", layer="iberian_penin_WGS84")

stder = st_read("D:/Salvatore_iberian_LPS/locations/iberian_penin_WGS84.shp")

igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
stder_igh <- st_transform(stder, igh)

bbox <- st_bbox(stder_igh)

ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
datos = 'ocs/ocs_0-30cm_mean.vrt'
file = "ocs_igh_0_5.tif"

gdal_translate(paste0(sg_url,datos), file ,
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh,
               verbose=TRUE)



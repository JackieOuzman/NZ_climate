
# libs <- c("RCurl", "dplyr", "tidyverse","geosphere")
# 
# install.libraries <- function(lib=NULL){
#   new <- lib[!(lib %in% installed.packages()[, "Package"])]
#   if (length(new)){   
#     install.packages(new, dependencies = TRUE)
#   }
# }
# 
# load.libraries <- function(lib=NULL){
#   sapply(libs, require, character.only = TRUE)
# }
# 
# install.libraries(libs)
# load.libraries(libs)




#install.packages("terra")

library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
library(rasterVis)

########################                      using Terra package                       ########################################
# load a climate and study area raster using Terra

#load a study raster (Rob created) # this is not what Rob created I have saved his grid with EPSG:2193 projection
study_vineyards <- rast("V:/Marlborough regional/Boundary_files/marlb1_reprojected.tif")
study_vineyards


#Climate grids
list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/")
#rainfall
list.files("V:/Marlborough regional/climate/wetransfer_AS/Rainfall_tiff/")

#### rainfall files
list_rain_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Rainfall_tiff/",
                             pattern = "rain")
list_rain_files



#### GST files
list_GST_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/",
                             pattern = "GST_Adel")
list_GST_files 



#### GDD files
list_GDD_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/",
                             pattern = "GDD10")
list_GDD_files 


#### veraison files
list_DOV_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/",
                             pattern = "Dateofveraison")
list_DOV_files 

#### flowering files
list_DOF_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/",
                             pattern = "DateofFlowering")
list_DOF_files         

#### harvest files
list_DOH_files <- list.files("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/",
            pattern = "Dateof200g")
list_DOH_files



list_files <- list_rain_files
list_files
########################################################################################################################
climate_metric <- "rain"
climate_metric
#string_extract <- "rain"

#start loop#
for (list_files in list_files){

#name_file_to_read <- paste0("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/", list_files)
name_file_to_read <- paste0("V:/Marlborough regional/climate/wetransfer_AS/Rainfall_tiff/", list_files)

climate_raster <- rast(name_file_to_read)

#climate_raster
# get the year from the file name Dateof200g20122013cor_SB_1.csv_B_gissmall_Ad

step1<-sub("cor.csv_B_Ad.tif*", "", list_files) # Extract characters before pattern
step1
year<-sub(".*rain", "", step1)                 # Extract characters after pattern
year
rm(step1)

## Project the data esure the climate data and the study area have the same EPSG 2193

#This should work
climate_raster_projected <- terra::project(x = climate_raster, 
                       y="epsg:2193", 
                       align= study_vineyards)

# change the resolution
climate_raster_projected_resample <- terra::resample(climate_raster_projected, 
                                  study_vineyards)

climate_raster_projected_resample


### mask

climate_raster_projected_resample_mask <- terra::mask(climate_raster_projected_resample,study_vineyards )
plot(climate_raster_projected_resample_mask)


terra::writeRaster(climate_raster_projected_resample_mask, 
                   paste0("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                          climate_metric,
                          "_proj_resample_mask_",
                          year,
                          ".tiff"),
                   overwrite=TRUE)
rm(climate_raster,climate_raster_projected, climate_raster_projected_resample, climate_raster_projected_resample_mask, year)
}               



### job for later work out how to plot using ggplot SpatRaster


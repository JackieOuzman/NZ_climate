
#install.packages("terra")



library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
#library(rasterVis)

########################                      using Terra package                       ########################################
# load a climate and study area raster using Terra
#which computer am I using - the path will be slightly different

path <- "V:/Viticulture/" #melb computer
#path <- "V:/" #my computer


#load a study raster (Rob created) # this is not what Rob created I have saved his grid with EPSG:2193 projection
#study_vineyards <- rast(paste0(path,    "Marlborough regional/Boundary_files/marlb1_reprojected.tif")) # I am not 100% happy with this
study_vineyards <- rast(paste0(path,    "Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m.tif")) # I think this is better
                         
study_vineyards
climate_grids <- "Marlborough regional/climate/climate_august2022/mix_file_revised/Climate_tiff/"





list_files <- list.files(paste0(path,climate_grids),
                             pattern = ".tif")

#list_files <- list_files[1]
########################################################################################################################


#start loop#
for (list_files in list_files){

name_file_to_read <-  paste0(path,climate_grids, list_files)

climate_raster <- rast(name_file_to_read)
#list_files

climate_metric <- stringr ::str_sub(list_files,1,3) # Extract characters before pattern GST and GDD 
#climate_metric

year<-stringr ::str_sub(list_files,5,12)
#year


## Project the data esure the climate data and the study area have the same EPSG 2193

#This should work
climate_raster
climate_raster_projected <- terra::project(x = climate_raster, 
                       y="epsg:2193")#, 
                       #align= study_vineyards)

# change the resolution
climate_raster_projected_resample <- terra::resample(climate_raster_projected, 
                                  study_vineyards)

climate_raster_projected_resample


### mask

climate_raster_projected_resample_mask <- terra::mask(climate_raster_projected_resample,study_vineyards )
plot(climate_raster_projected_resample_mask)


terra::writeRaster(climate_raster_projected_resample_mask, 
                   paste0(path,"Marlborough regional/climate/climate_data_2022_vineyards_R_august2022/",
                          climate_metric,
                          "_proj_resample_mask_",
                          year,
                          ".tiff"),
                   overwrite=TRUE)
rm(climate_raster,climate_raster_projected, climate_raster_projected_resample, climate_raster_projected_resample_mask, year)
}               





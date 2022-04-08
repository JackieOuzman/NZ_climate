
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




install.packages("terra")

library(terra)
#library(raster)
library(ggplot2)


#lets load a climate raster

DOH_2013_name <-"V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/Dateof200g20122013cor_SB_1.csv_B_gissmall_Ad.tif"
DOH_2013 <- rast(DOH_2013_name)

DOH_2013
crs(DOH_2013)


#lets load a study raster (Rob created)
study_vineyards <- rast("V:/Marlborough regional/Boundary_files/marlb1_reprojected.tif")
study_vineyards



## Project the data esure the climate data and the study area have the same EPSG 2193

#This should work
test <- terra::project(DOH_2013, y="epsg:2193", align= study_vineyards)
plot(test)


terra::crs(DOH_2013)  <-"epsg: 2193"
terra::crs(study_vineyards) <- "epsg: 2193"

study_vineyards
DOH_2013


#1, deal with the resolution

terra::res(DOH_2013)
terra::res(study_vineyards)
crs(study_vineyards)


test <- terra::resample(DOH_2013, study_vineyards)
test








#??test_resolution_DOH_2013 <- projectRaster(DOH_2013,
                                       #  crs = crs(study_vineyards),
                                       # res = res(study_vineyards)) 




ggplot() +
  geom_raster(data = DOH_2013 , 
              aes(x = x, y = y, 
                  fill = DOH_2013)) + 
  geom_raster(data = study_vineyards, 
              aes(x = x, y = y, 
                  alpha = study_vineyards)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()




### Now 'resample' so that the climate data has same resolution as the study_vineyards
#https://rspatial.org/terra/pkg/5-methods.html
#aggregate and disagg allow for changing the resolution (cell size) of a SpatRaster object. 
#In the case of aggregate, you need to specify a function determining what to do with the grouped cell values (e.g. mean). 
#It is possible to specify different (dis)aggregation factors in the x and y direction. 
#aggregate and disagg are the best methods when adjusting cells size only, 
#with an integer step (e.g. each side 2 times smaller or larger), 
#but in some cases that is not possible.

## the climate raster has cell size 0.002417778, 0.001822037
## while the study vineyards has cell size 100, 100

plot(DOH_2013)
ra <- aggregate(DOH_2013, 2, fun = "mean")

ra
plot(ra)

### Crop the spatial extent
test_crop <- terra::crop(DOH_2013, study_vineyards, mask=TRUE)

### doing what rob wants grid * climate
study_times_climate <- study_vineyards * DOH_2013




#NOW I want
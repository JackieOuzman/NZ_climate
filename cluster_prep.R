
library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
library(rasterVis)

### bring in the GST grids and extract values for each grid pt
# I havent work out how to do the sample function so I have done this in ESRI
grid_pts_CSV <- read.csv("V:/Marlborough regional/Boundary_files/Grid_pts.csv")

grid_pts_CSV$ID <- seq.int(nrow(grid_pts_CSV))
names(grid_pts_CSV)
grid_pts_CSV <- grid_pts_CSV %>%  dplyr::select("X","Y", "ID" )


## UP TO HERE
station_pts <- vect(longlat, crs = "+proj=longlat +datum=WGS84")


grid_pts <- terra::vect("V:/Marlborough regional/Boundary_files/Grid_pts.csv")



rain_template <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20122013.tiff")

terra::extract(rain_template, rain_template)


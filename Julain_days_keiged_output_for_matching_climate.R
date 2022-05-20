library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
library(rasterVis)
library(tidyverse)

### bring in the GST grids and extract values for each grid pt
# I havent work out how to do the sample function so I have done this in ESRI



###############################################################################################
###  Bring in the points for every cell in raster with also with the subregions attached

#grid_pts <- terra::vect("V:/Viticulture/Marlborough regional/Boundary_files/temp/Grid_pts_sub_regions.shp") #melb computer
grid_pts <- terra::vect("V:/Marlborough regional/Boundary_files/temp/Grid_pts_sub_regions.shp") #my computer
plot(grid_pts)
grid_pts

#convert the pts to a dataframe BUT i want more decimal places!!
grid_pts_df <- as.data.frame(grid_pts)


## bring in the Kriged harvest dates raster
kriged_data_path <- "V:/Marlborough regional/working_jaxs/for_mapping_may2022"
Julain_2014 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2014",
                           "/Vesper/",
                           "julian_days_2014_HighDensity_Julian_201_PRED_100m.tif")) 

Julain_2015 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2015",
                           "/Vesper/",
                           "julian_days_2015_HighDensity_Julian_201_PRED_100m.tif"))

Julain_2016 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2016",
                           "/Vesper/",
                           "julian_days_2016_HighDensity_Julian_201_PRED_100m.tif"))
Julain_2017 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2017",
                           "/Vesper/",
                           "julian_days_2017_HighDensity_Julian_201_PRED_100m.tif"))

Julain_2018 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2018",
                           "/Vesper/",
                           "julian_days_2018_HighDensity_Julian_201_PRED_100m.tif"))

Julain_2019 <- rast(paste0(kriged_data_path,
                           "/vesper_Julian_days/Julian",
                           "2019",
                           "/Vesper/",
                           "julian_days_2019_HighDensity_Julian_201_PRED_100m.tif"))


## extract the points for the cliamte raster
Julain2014_df <- terra::extract(  x = Julain_2014,
                            y = grid_pts,
                            xy=TRUE)
colnames(Julain2014_df) <- c("ID","Julain_2014","x", "y" ) 
Julain2014_df <- Julain2014_df %>% dplyr::mutate(year = "2014",
                                     for_join = paste0(x, "_", y))


Julain2015_df <- terra::extract(  x = Julain_2015,
                                  y = grid_pts,
                                  xy=TRUE)
colnames(Julain2015_df) <- c("ID","Julain_2015","x", "y" ) 
Julain2015_df <- Julain2015_df %>% dplyr::mutate(year = "2015",
                                                 for_join = paste0(x, "_", y))


Julain2016_df <- terra::extract(  x = Julain_2016,
                                  y = grid_pts,
                                  xy=TRUE)
colnames(Julain2016_df) <- c("ID","Julain_2016","x", "y" ) 
Julain2016_df <- Julain2016_df %>% dplyr::mutate(year = "2016",
                                                 for_join = paste0(x, "_", y))


Julain2017_df <- terra::extract(  x = Julain_2017,
                                  y = grid_pts,
                                  xy=TRUE)
colnames(Julain2017_df) <- c("ID","Julain_2017","x", "y" ) 
Julain2017_df <- Julain2017_df %>% dplyr::mutate(year = "2017",
                                                 for_join = paste0(x, "_", y))


Julain2018_df <- terra::extract(  x = Julain_2018,
                                  y = grid_pts,
                                  xy=TRUE)
colnames(Julain2018_df) <- c("ID","Julain_2018","x", "y" ) 
Julain2018_df <- Julain2018_df %>% dplyr::mutate(year = "2018",
                                                 for_join = paste0(x, "_", y))


Julain2019_df <- terra::extract(  x = Julain_2019,
                                  y = grid_pts,
                                  xy=TRUE)
colnames(Julain2019_df) <- c("ID","Julain_2019","x", "y" ) 
Julain2019_df <- Julain2019_df %>% dplyr::mutate(year = "2019",
                                                 for_join = paste0(x, "_", y))


### join them all together
names(Julain2014_df)
Julain2014_df <- Julain2014_df %>% 
  select(for_join, Julain_2014)
Julain2015_df <- Julain2015_df %>% 
  select(for_join, Julain_2015)

Julain_df <- left_join(Julain2014_df, Julain2015_df)

Julain2016_df <- Julain2016_df %>% 
  select(for_join, Julain_2016)

Julain_df <- left_join(Julain_df, Julain2016_df)

Julain2017_df <- Julain2017_df %>% 
  select(for_join, Julain_2017)

Julain_df <- left_join(Julain_df, Julain2017_df)

Julain2018_df <- Julain2018_df %>% 
  select(for_join, Julain_2018)

Julain_df <- left_join(Julain_df, Julain2018_df)


Julain2019_df <- Julain2019_df %>% 
  select(for_join, Julain_2019)

Julain_df <- left_join(Julain_df, Julain2019_df)




grid_pts_df <- grid_pts_df %>% dplyr::mutate(for_join = paste0(X, "_", Y))

Julain_df <- left_join(Julain_df, grid_pts_df)


write.csv(Julain_df, "V:/Marlborough regional/working_jaxs/for_mapping_may2022/vesper_Julian_days/Kriged_julian_days_pts_JMPIN.csv",
          row.names = FALSE)

rm(list=ls()[ls()!= "Julain_df"])
names(Julain_df)
## just check that it ok - it does
Julain_df %>% 
ggplot(aes(x= X, y = Y))+ 
  geom_point()
#### modelled data

climate_grid_values <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/Climate_all_JMP_IN.csv")
names(climate_grid_values)
climate_grid_values_DOH <- climate_grid_values %>% 
dplyr::select(for_join,DOH_2013:DOH_2020)
names(climate_grid_values_DOH)
## just check that it ok - it does
climate_grid_values %>% 
  ggplot(aes(x= X, y = Y))+ 
  geom_point()

### join it to the Julain_df

Julain_df_plus_climate <- left_join(Julain_df, climate_grid_values_DOH)

names(Julain_df_plus_climate)
## plot
## just check that it ok - it does
Julain_df_plus_climate %>% 
  ggplot(aes(x= X, y = Y))+ 
  geom_point()

Julain_df_plus_climate %>%  
  ggplot( mapping = aes(Julain_2014, DOH_2014)) +
  geom_point() +
    theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Modelled climate data vs collected data",
       x = "Observed Day of harvest", y = "Modlled day of harvest")

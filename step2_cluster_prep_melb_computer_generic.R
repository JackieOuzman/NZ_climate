
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
# this was done in Arcmap "V:\Marlborough regional\Council\Regional_Grid\extracting_pts_frm_base_grid.mxd"

path <- "V:/Viticulture/" #melb computer

#grid_pts_df <- read.csv("V:/Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv")
grid_pts_df <- read.csv(paste0(path,"Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv"))

grid_pts_vec <- terra::vect( grid_pts_df, geom=c('X','Y'), crs=paste0("epsg:",2193) )
plot(grid_pts_vec)





## list in the climate data
file_list <- list.files(paste0(path, "Marlborough regional/climate/climate_data_2022_vineyards_R_august2022/"),  pattern = ".tif")#Melb   computer


### ---- TEST ---####
#file_list <-file_list[63]
### ---- END OF TEST ---####

# file_list_GDD <- list.files(paste0(path, "Marlborough regional/climate/climate_data_2022_vineyards_R_august2022/"),  pattern = "GDD")
# file_list_GDD [1]
# file_list <- file_list_GDD

## this will be the loop
for (file_list in file_list){

  
climate_type <- stringr ::str_sub(file_list,1,3) # Extract characters before pattern GST and GDD 
climate_type
  
year<-stringr ::str_sub(file_list,28,31)
year
 


## bring in the climate raster 

climate <- rast(paste0(path,"Marlborough regional/climate/climate_data_2022_vineyards_R_august2022/",
                         file_list)) #melb computer

  
  ## extract the points for the cliamte raster
  climate <- terra::extract(  x = climate,
                              y = grid_pts_vec,
                              xy=TRUE)
  
colnames(climate) <- c("ID",climate_type,"x", "y" ) 
climate <- climate %>% dplyr::mutate(year = year,
                                     for_join = paste0(x, "_", y))
grid_pts_df <- grid_pts_df %>% dplyr::mutate(for_join = paste0(X, "_", Y))


climate <- left_join(climate, grid_pts_df)
climate <- climate %>% 
  dplyr::select(-Rowid_,
         - MARL_AG_200_BLOC,
         - x,
         -y,
         -MARL_AG_200_BL_1)
  
name <- paste0(climate_type,"_", year)
assign(name,climate)  
rm(climate, name,year)

} 

climate_all_rain <- rbind(
  rai_2013,
  rai_2014,
  rai_2015,
  rai_2016,
  rai_2017,
  rai_2018,
  rai_2019,
  rai_2020,
  rai_2021
  )

rm(rai_2013,
   rai_2014,
   rai_2015,
   rai_2016,
   rai_2017,
   rai_2018,
   rai_2019,
   rai_2020,
   rai_2021)

climate_all_GST <- rbind(
  GST_2013,
  GST_2014,
  GST_2015,
  GST_2016,
  GST_2017,
  GST_2018,
  GST_2019,
  GST_2020,
  GST_2021
  )

rm(GST_2013,
GST_2014,
GST_2015,
GST_2016,
GST_2017,
GST_2018,
GST_2019,
GST_2020,
GST_2021)

climate_all_GDD <- rbind(
  GDD_2013,
  GDD_2014,
  GDD_2015,
  GDD_2016,
  GDD_2017,
  GDD_2018,
  GDD_2019,
  GDD_2020,
  GDD_2021
  )

rm(GDD_2013,
GDD_2014,
GDD_2015,
GDD_2016,
GDD_2017,
GDD_2018,
GDD_2019,
GDD_2020,
GDD_2021)

climate_all_DOH <- rbind(
  DOH_2013,
  DOH_2014,
  DOH_2015,
  DOH_2016,
  DOH_2017,
  DOH_2018,
  DOH_2019,
  DOH_2020,
  DOH_2021
)

rm(DOH_2013,
   DOH_2014,
   DOH_2015,
   DOH_2016,
   DOH_2017,
   DOH_2018,
   DOH_2019,
   DOH_2020,
   DOH_2021)


climate_all_DOF <- rbind(
  DOF_2013,
  DOF_2014,
  DOF_2015,
  DOF_2016,
  DOF_2017,
  DOF_2018,
  DOF_2019,
  DOF_2020,
  DOF_2021
)

rm(DOF_2013,
   DOF_2014,
   DOF_2015,
   DOF_2016,
   DOF_2017,
   DOF_2018,
   DOF_2019,
   DOF_2020,
   DOF_2021)

climate_all_DOV <- rbind(
  DOV_2013,
  DOV_2014,
  DOV_2015,
  DOV_2016,
  DOV_2017,
  DOV_2018,
  DOV_2019,
  DOV_2020,
  DOV_2021
)

rm( DOV_2013,
    DOV_2014,
    DOV_2015,
    DOV_2016,
    DOV_2017,
    DOV_2018,
    DOV_2019,
    DOV_2020,
    DOV_2021)

#########################################################################################

names(climate_all_rain)


### make dataset wider for each climate variable
climate_all_rain_wider <- climate_all_rain %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "rai_",
    values_from = rai)

climate_all_GST_wider <- climate_all_GST %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "GST_",
    values_from = GST)

climate_all_GDD_wider <- climate_all_GDD %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "GDD_",
    values_from = GDD)

climate_all_DOF_wider <- climate_all_DOF %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "DOF_",
    values_from = DOF)

climate_all_DOV_wider <- climate_all_DOV %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "DOV_",
    values_from = DOV)

climate_all_DOH_wider <- climate_all_DOH %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "DOH_",
    values_from = DOH)

climate_all <- left_join(climate_all_rain_wider,
                         climate_all_GST_wider)

climate_all <- left_join(climate_all,
                         climate_all_GDD_wider)

climate_all <- left_join(climate_all,
                         climate_all_DOF_wider)

climate_all <- left_join(climate_all,
                         climate_all_DOV_wider)

climate_all <- left_join(climate_all,
                         climate_all_DOH_wider)

### append the climate wider data to the subregions


#######################################################################################################################################
#######################################      save the outputs       ################################################################### 
#######################################################################################################################################

names(climate_all)



write.csv(
  climate_all,
  paste0(
    path,
    "Marlborough regional/climate/climate_data_2022_vineyards_R_august2022/Climate_data_as_pts_august2022/Climate_data_as_pts_august2022",
    "climate_all_cluster_input.csv"),
          row.names = FALSE) 







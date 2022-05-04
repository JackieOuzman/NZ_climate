
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






## list in the climate data
list.files("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/")

climate_type <-"rain"     #"rain""GST""GDD" "DOH"DOF""DOV"

climate_files <- list.files("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/",
           pattern = climate_type)
View(climate_files)

climate_files <- as.data.frame(climate_files)

climate_tiff <- climate_files %>% 
  dplyr::filter(str_detect(climate_files,".tiff")) %>% 
  dplyr::filter(str_detect(climate_files,"aux.xml", negate = TRUE))

climate_tiff$climate_files <- as.character(climate_tiff$climate_files)
#Turn into a list

file_list <- climate_tiff[["climate_files"]] 

file_list

#file_list <- file_list[1]

## this will be the loop
for (file_list in file_list){

  
## get the year from the file name
  #step1<-sub(".tiff*", "", file_list) # Extract characters before pattern # for the rainfall files AND doh
  step1<-sub("cor.csv_B_gissmall_Ad.tif.tiff*", "", file_list) # Extract characters before pattern  # for the GST AND gdd files
  bit_to_extract <- paste0(".*",climate_type,"_proj_resample_mask_" )
  year2 <-sub(bit_to_extract, "", step1)# Extract characters after pattern
  year <- str_sub(year2,-4,-1)
  rm(step1, year2, bit_to_extract)

## bring in the climate raster 
  climate <- rast(paste0("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                         file_list)) 
## extract the points for the cliamte raster
  climate <- terra::extract(  x = climate,
                              y = grid_pts,
                              xy=TRUE)
  
colnames(climate) <- c("ID",climate_type,"x", "y" ) 
climate <- climate %>% dplyr::mutate(year = year,
                                     for_join = paste0(x, "_", y))
grid_pts_df <- grid_pts_df %>% dplyr::mutate(for_join = paste0(X, "_", Y))


climate <- left_join(climate, grid_pts_df)
  
name <- paste0(climate_type,"_", year)
assign(name,climate)  
rm(climate, name,year)
  
} 

# climate_all <- rbind(
#   rain_2013,
#   rain_2014,
#   rain_2015,
#   rain_2016,
#   rain_2017,
#   rain_2018,
#   rain_2019,
#   rain_2020,
#   rain_2021
#   )

climate_all <- rbind(
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

rm(DOV_2013,
   DOV_2014,
   DOV_2015,
   DOV_2016,
   DOV_2017,
   DOV_2018,
   DOV_2019,
   DOV_2020,
   DOV_2021)

str(climate_all)
## tidy up a bit...
climate_all <- climate_all %>% 
  dplyr::select(-x, -y)

### make dataset wider
climate_all_wider <- climate_all %>% 
  pivot_wider(
    names_from = year,
    #values_from = rain)
    values_from = climate_type)

### append the climate wider data to the subregions

#change year to double to get geom_smooth to work
str(climate_all)
climate_all <- mutate(climate_all, year_as_double = as.double(year))


unique(climate_all$Region_Nam)

## modify the regions
climate_all <- climate_all %>% 
  mutate(merged_regions = case_when(
    Region_Nam == "Wairau Plain (East of Narrows)" ~ "Wairau Valley and Plain",
    Region_Nam == "Wairau Valley (West of Narrows)" ~ "Wairau Valley and Plain",
    Region_Nam == "Awatere" ~ "Awatere and South",
    Region_Nam == "Grassmere, Flaxbourne, Kekerengu" ~ "Awatere and South",
    Region_Nam == "" ~ "Not defined"
  ))

#######################################################################################################################################
#######################################      save the outputs       ################################################################### 
#######################################################################################################################################

write.csv(
  climate_all,
  paste0(
    "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
    climate_type,
    "_climate_all.csv"),
    row.names = FALSE
  )
write.csv(
  climate_all_wider,
  paste0(
    "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
    climate_type,
    "_climate_all_cluster_input.csv"),
          row.names = FALSE) 

str(climate_all)
climate_type





#####################################################################################################################


#https://rpubs.com/odenipinedo/cluster-analysis-in-R
climate_all_wider <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_climate_all_cluster_input.csv")
  
 

#### Clusering using R data has no transformations - not sure if it needs to?
library(cluster)

####################################################################################
####    INPUT DATA    ##############################################################

##make a dataset we can work with
str(climate_all_wider)
cluser_input <- climate_all_wider %>% 
  dplyr::select("X2013":"X2021")


####################################################################################
####    Scale DATA    ##############################################################

cluser_input_scale <- scale(cluser_input)

####################################################################################
####    HOW MANY CLUSTERS    #######################################################

## elbow plot
library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = cluser_input_scale, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)



###  silhouette width taking ages - it does!


sil_width <- map_dbl(2:10,function(k){
  model <- cluster::pam(x = cluser_input_scale, k = k)  
  model$silinfo$avg.width})

sil_df <- data.frame(
  k =2:10,  
  sil_width = sil_width)



# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Plot the silhouette width Note higher sil width indicated better fit of model
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 1:5)




####################################################################################
####    HOW MANY CLUSTERS SAVE OUTPUT   #############################################

how_many_k <- left_join(elbow_df, sil_df)
how_many_k

write.csv(how_many_k,
          paste0(
          "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
          "rainfall_scaled",
          #climate_type,
          "_how_many_k.csv"),
          row.names = FALSE) 


### Run solution from above analysis

## best number of clusters are:
k_mean_number <- 7


kmean_model <- kmeans(cluser_input_scale, centers = k_mean_number)

climate_all_wider <- climate_all_wider %>% 
  dplyr::mutate(k_cluster  = kmean_model$cluster)

names(climate_all_wider)

### Plot and check to solution looks good

climate_all_wider %>% 
  ggplot(aes(x = X , y = Y, colour = factor(k_cluster)))+
  geom_point()




write.csv(climate_all_wider,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            climate_type,
            "_kmean.csv"),
          row.names = FALSE) 
          
          

## stats on the cluster solution



str(climate_all_wider)

Kmean_2Clust_mean <- climate_all_wider %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(`2013`:`2021`),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )

Kmean_2Clust_SD<- climate_all_wider %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(`2013`:`2021`),sd, na.rm = TRUE)%>% 
  dplyr::mutate(stats = "SD" )

Kmean_2Clust_mean
Kmean_2Clust_SD
Kmean_2Clust_stats <- rbind(Kmean_2Clust_mean, Kmean_2Clust_SD)
Kmean_2Clust_stats

Kmean_2Clust_count<- climate_all_wider %>% 
  group_by(k_cluster) %>% 
  summarise(count = n())

Kmean_2Clust_stats <- left_join(Kmean_2Clust_stats,Kmean_2Clust_count)
Kmean_2Clust_stats


write.csv(Kmean_2Clust_stats,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            climate_type,
            "_kmean_stats.csv"),
          row.names = FALSE) 

          
          


                                
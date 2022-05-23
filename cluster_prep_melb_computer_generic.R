
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

grid_pts_df <- read.csv("V:/Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv")

grid_pts_vec <- vect( grid_pts_df, geom=c('X','Y'), crs=paste0("epsg:",2193) )
plot(grid_pts_vec)






## list in the climate data
#list.files("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/") #Melb   computer
list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/") #Jackies computer

climate_type <-"DOV"     #"rain""GST""GDD" "DOH"DOF""DOV"

# climate_files <- list.files("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/",
#            pattern = climate_type)
climate_files <- list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                            pattern = climate_type)

#View(climate_files)

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
  step1<-sub(".tiff*", "", file_list) # Extract characters before pattern # for GST
  
  
  
  bit_to_extract <- paste0(".*",climate_type,"_proj_resample_mask_" )
  year2 <-sub(bit_to_extract, "", step1)# Extract characters after pattern
  year <- str_sub(year2,-4,-1)
  rm(step1, year2, bit_to_extract)

## bring in the climate raster 
  # climate <- rast(paste0("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/",
  #                        file_list)) #melb computer
  climate <- rast(paste0("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                         file_list)) #my computer
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
  select(-Rowid_,
         - MARL_AG_200_BLOC,
         - x,
         -y,
         -MARL_AG_200_BL_1)
  
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
# 
# rm(rain_2013,
#    rain_2014,
#    rain_2015,
#    rain_2016,
#    rain_2017,
#    rain_2018,
#    rain_2019,
#    rain_2020,
#    rain_2021)

# climate_all_GST <- rbind(
#   GST_2013,
#   GST_2014,
#   GST_2015,
#   GST_2016,
#   GST_2017,
#   GST_2018,
#   GST_2019,
#   GST_2020,
#   GST_2021
#   )
#  
# rm(GST_2013,
# GST_2014,
# GST_2015,
# GST_2016,
# GST_2017,
# GST_2018,
# GST_2019,
# GST_2020,
# GST_2021)

# climate_all_GDD <- rbind(
#   GDD_2013,
#   GDD_2014,
#   GDD_2015,
#   GDD_2016,
#   GDD_2017,
#   GDD_2018,
#   GDD_2019,
#   GDD_2020,
#   GDD_2021
#   )
#   
# rm(GDD_2013,
# GDD_2014,
# GDD_2015,
# GDD_2016,
# GDD_2017,
# GDD_2018,
# GDD_2019,
# GDD_2020,
# GDD_2021)

# climate_all_DOH <- rbind(
#   DOH_2013,
#   DOH_2014,
#   DOH_2015,
#   DOH_2016,
#   DOH_2017,
#   DOH_2018,
#   DOH_2019,
#   DOH_2020,
#   DOH_2021
# )
# 
# rm(DOH_2013,
#    DOH_2014,
#    DOH_2015,
#    DOH_2016,
#    DOH_2017,
#    DOH_2018,
#    DOH_2019,
#    DOH_2020,
#    DOH_2021)


# climate_all_DOF <- rbind(
#   DOF_2013,
#   DOF_2014,
#   DOF_2015,
#   DOF_2016,
#   DOF_2017,
#   DOF_2018,
#   DOF_2019,
#   DOF_2020,
#   DOF_2021
# )
# 
# rm(DOF_2013,
#    DOF_2014,
#    DOF_2015,
#    DOF_2016,
#    DOF_2017,
#    DOF_2018,
#    DOF_2019,
#    DOF_2020,
#    DOF_2021)

# climate_all_DOV <- rbind(
#   DOV_2013,
#   DOV_2014,
#   DOV_2015,
#   DOV_2016,
#   DOV_2017,
#   DOV_2018,
#   DOV_2019,
#   DOV_2020,
#   DOV_2021
# )
# 
# rm(DOV_2013,
#    DOV_2014,
#    DOV_2015,
#    DOV_2016,
#    DOV_2017,
#    DOV_2018,
#    DOV_2019,
#    DOV_2020,
#    DOV_2021)

#########################################################################################




### make dataset wider for each climate variable
climate_all_rain_wider <- climate_all %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "rain_",
    values_from = rain)

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
    "V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
    climate_type,
    "_climate_all_cluster_input.csv"),
          row.names = FALSE) 






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

          
          


                                
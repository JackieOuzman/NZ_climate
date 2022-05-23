
library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
library(rasterVis)
library(tidyverse)
library(cluster)
library(purrr)



#######################################################################################################################################
#######################################      bring in the climate data       ################################################################### 
####################################### this is extracted pt data see step 2 script ################################################################### 
#######################################################################################################################################


climate_pre_cluster <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_all_cluster_input.csv")

names(climate_pre_cluster)
#####################################################################################################################

#https://rpubs.com/odenipinedo/cluster-analysis-in-R

  
#### Clusering using R data has no transformations - not sure if it needs to?

####################################################################################
####    INPUT DATA    ##############################################################

##make a dataset we can work with
str(climate_pre_cluster)
cluser_input_rain <- climate_pre_cluster %>% 
  dplyr::select("rain_2013":"rain_2021", X, Y)

#remove the missing values

cluser_input_rain <- cluser_input_rain %>% 
  filter(!is.na(rain_2013))

####################################################################################
####    Scale DATA    ##############################################################

cluser_input_rain_scale <- scale(cluser_input_rain)

####################################################################################
####    HOW MANY CLUSTERS    #######################################################

## elbow plot
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = cluser_input_rain_scale, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)



###  silhouette width taking ages - it does!


sil_width <- map_dbl(2:10,function(k){
  model <- cluster::pam(x = cluser_input_rain_scale, k = k)  
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

# how_many_k <- left_join(elbow_df, sil_df)
# how_many_k
# 
# write.csv(how_many_k,
#           paste0(
#           "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
#           "rainfall_scaled",
#           #climate_type,
#           "_how_many_k.csv"),
#           row.names = FALSE) 


### Run solution from above analysis

## best number of clusters are:
k_mean_number <- 7


kmean_model <- kmeans(cluser_input_rain_scale, centers = k_mean_number)
#add the cluster solution to the cliamte data
cluser_input_rain <- cluser_input_rain %>% 
  dplyr::mutate(k_cluster  = kmean_model$cluster)

names(cluser_input_rain)

### Plot and check to solution looks good

cluser_input_rain %>% 
  ggplot(aes(x = X , y = Y, colour = factor(k_cluster)))+
  geom_point()




write.csv(cluser_input_rain,
          paste0(
            "V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/R_cluster_solution_option/",
            "rain_7Clusters",
            "_kmean.csv"),
          row.names = FALSE) 
          
          

## stats on the cluster solution



str(cluser_input_rain)

Kmean_7Clust_mean <- cluser_input_rain %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(rain_2013:rain_2021),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )

Kmean_7Clust_SD<- cluser_input_rain %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(rain_2013:rain_2021),sd, na.rm = TRUE)%>% 
  dplyr::mutate(stats = "SD" )

Kmean_7Clust_mean
Kmean_7Clust_SD
Kmean_7Clust_stats <- rbind(Kmean_7Clust_mean, Kmean_7Clust_SD)
Kmean_7Clust_stats

Kmean_7Clust_count<- cluser_input_rain %>% 
  group_by(k_cluster) %>% 
  summarise(count = n())

Kmean_7Clust_stats <- left_join(Kmean_7Clust_stats,Kmean_7Clust_count)
Kmean_7Clust_stats


write.csv(Kmean_7Clust_stats,
          paste0(
            "V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/R_cluster_solution_option/",
            "rain_7_culster",
            "_kmean_stats.csv"),
          row.names = FALSE) 

          
          


                                
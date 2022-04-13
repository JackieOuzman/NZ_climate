
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
grid_pts <- terra::vect("V:/Marlborough regional/Boundary_files/temp/Grid_pts_sub_regions.shp")
plot(grid_pts)

list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
           pattern = "rain")

rain_2013 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20122013.tiff")
rain_2014 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20132014.tiff")
rain_2015 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20142015.tiff")
rain_2016 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20152016.tiff")
rain_2017 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20162017.tiff")
rain_2018 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20172018.tiff")
rain_2019 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20182019.tiff")
rain_2020 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20192020.tiff")
rain_2021 <- rast("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/rain_proj_resample_mask_20202021.tiff")

rain_2013 <- terra::extract(x = rain_2013,
                            y = grid_pts,
                            xy=TRUE)
rain_2014 <- terra::extract(x = rain_2014,
                               y = grid_pts,
                               xy=TRUE)
rain_2015 <- terra::extract(x = rain_2015,
                            y = grid_pts,
                            xy=TRUE)
rain_2016 <- terra::extract(x = rain_2016,
                            y = grid_pts,
                            xy=TRUE)
rain_2017 <- terra::extract(x = rain_2017,
                            y = grid_pts,
                            xy=TRUE)
rain_2018 <- terra::extract(x = rain_2018,
                            y = grid_pts,
                            xy=TRUE)
rain_2019 <- terra::extract(x = rain_2019,
                            y = grid_pts,
                            xy=TRUE)
rain_2020 <- terra::extract(x = rain_2020,
                            y = grid_pts,
                            xy=TRUE)
rain_2021 <- terra::extract(x = rain_2021,
                            y = grid_pts,
                            xy=TRUE)


rain_all <- left_join(rain_2013, rain_2014)
rain_all <- left_join(rain_all, rain_2015)
rain_all <- left_join(rain_all, rain_2016)
rain_all <- left_join(rain_all, rain_2017)
rain_all <- left_join(rain_all, rain_2018)
rain_all <- left_join(rain_all, rain_2019)
rain_all <- left_join(rain_all, rain_2020)
rain_all <- left_join(rain_all, rain_2021)

 
names(rain_all)
rain_all <- rain_all %>% rename(
  `2013` = rain_proj_resample_mask_20122013,
  `2014` = rain_proj_resample_mask_20132014,
  `2015` = rain_proj_resample_mask_20142015,
  `2016` = rain_proj_resample_mask_20152016,
  `2017` = rain_proj_resample_mask_20162017,
  `2018` = rain_proj_resample_mask_20172018,
  `2019` = rain_proj_resample_mask_20182019,
  `2020` = rain20192020cor.csv_B_Ad,
  `2021` = rain_proj_resample_mask_20202021
)


#convert the pts to a dataframe BUT i want more decimal places!!
grid_pts_df <- as.data.frame(grid_pts)

# test <- terra::geom(grid_pts, wkt=TRUE) # this is not quite what I want
# head(test)

rain_all_coords_sub_regions <- cbind(rain_all,grid_pts_df)
str(rain_all_coords_sub_regions)
rain_all_coords_sub_regions <- rain_all_coords_sub_regions %>% 
  dplyr::select(ID, X, Y ,Region_Nam,OBJECTID, `2013`, `2014`: `2021` )

### make dataset narrow
rain_all_coords_sub_regions_narrow <- rain_all_coords_sub_regions %>% 
  pivot_longer(
    `2013`:`2021`, 
  names_to = "year", 
  values_to = "mean_rainfall")

#change year to double to get geom_smooth to work
rain_all_coords_sub_regions_narrow <- mutate(rain_all_coords_sub_regions_narrow, year_as_double = as.double(year))

str(rain_all_coords_sub_regions_narrow)
unique(rain_all_coords_sub_regions_narrow$Region_Nam)

## modify the regions
rain_all_coords_sub_regions_narrow <- rain_all_coords_sub_regions_narrow %>% 
  mutate(merged_regions = case_when(
    Region_Nam == "Wairau Plain (East of Narrows)" ~ "Wairau Valley and Plain",
    Region_Nam == "Wairau Valley (West of Narrows)" ~ "Wairau Valley and Plain",
    Region_Nam == "Awatere" ~ "Awatere and South",
    Region_Nam == "Grassmere, Flaxbourne, Kekerengu" ~ "Awatere and South",
    Region_Nam == "" ~ "Not defined"
  ))


#plots the results by year:
rain_all_coords_sub_regions_narrow %>% 
  filter(merged_regions != "Not defined") %>% 
ggplot(aes(factor(year_as_double), mean_rainfall, colour= merged_regions))+
  geom_boxplot()+
  facet_wrap(.~merged_regions)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "GS rainfall",
       title = "Sample points over the region")
#subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. 
#")

rain_all_coords_sub_regions_narrow %>% 
  filter(merged_regions != "Not defined") %>% 
  ggplot(aes(merged_regions, mean_rainfall, colour= merged_regions))+
  geom_boxplot()+
  facet_wrap(.~ factor(year_as_double))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "GS rainfall",
       title = "Sample points over the region")
#subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. 
#")


rain_all_coords_sub_regions_narrow %>% 
  filter(merged_regions != "Not defined") %>% 
  ggplot(aes( mean_rainfall))+
  geom_histogram()+
  facet_wrap(.~merged_regions)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "GS rainfall",
       title = "Sample points over the region")
#subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. 
#")

rain_all_coords_sub_regions_narrow %>% 
  #filter(merged_regions != "Not defined") %>% 
  ggplot(aes( mean_rainfall))+
  geom_histogram()+
  facet_wrap(.~factor(year_as_double))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "GS rainfall",
       title = "Sample points over the region")
#subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. 
#")




#####################################################################################################################


#https://rpubs.com/odenipinedo/cluster-analysis-in-R



#### Clusering
library(cluster)
##make a dataset we can work with
str(rain_all_coords_sub_regions)
cluser_input <- rain_all_coords_sub_regions %>% 
  select(`2013`:`2021`  )
kmean_2 <- kmeans(cluser_input, centers = 2)

rain_all_coords_sub_regions_temp <- rain_all_coords_sub_regions %>% 
  mutate(K_Cluster_2 = kmean_2$cluster)
names(rain_all_coords_sub_regions_temp)

rain_all_coords_sub_regions_temp %>% 
  ggplot(aes(x = X , y = Y, colour = factor(K_Cluster_2)))+
  geom_point()+
  facet_wrap(.~Region_Nam)

## elbow plot
library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = cluser_input, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

###  silhouette width taking ages?? something wrong
sil_width <- map_dbl(2:10,function(k){
  model <- pam(x = cluser_input, k = k)  
  model$silinfo$avg.width})

sil_df <- data.frame(
  k =2:10,  
  sil_width = sil_width)

sil_df

# Plot the silhouette width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)


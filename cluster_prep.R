
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
#convert the pts to a dataframe BUT i want more decimal places!!
grid_pts_df <- as.data.frame(grid_pts)






## list in the climate data
climate_files <- list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
           pattern = "rain")
#View(climate_files)

climate_files <- as.data.frame(climate_files)

climate_tiff <- climate_files %>% 
  dplyr::filter(str_detect(climate_files,".tiff")) %>% 
  dplyr::filter(str_detect(climate_files,"aux.xml", negate = TRUE))

climate_tiff$climate_files <- as.character(climate_tiff$climate_files)
#Turn into a list

file_list <- climate_tiff[["climate_files"]] 
#file_list <- "rain_proj_resample_mask_20122013.tiff"


climate_type <- "rain"

## this will be the loop
for (file_list in file_list){

  
## get the year from the file name
  step1<-sub(".tiff*", "", file_list) # Extract characters before pattern
  bit_to_extract <- paste0(".*",climate_type,"_proj_resample_mask_" )
  year2 <-sub(bit_to_extract, "", step1)# Extract characters after pattern
  year <- str_sub(year2,-4,-1)
  rm(step1, year2, bit_to_extract)

## bring in the climate raster 
  climate <- rast(paste0("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
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

climate_all <- rbind(
  rain_2013,
  rain_2014,
  rain_2015,
  rain_2016,
  rain_2017,
  rain_2018,
  rain_2019,
  rain_2020,
  rain_2021
  )


rm(rain_2013,
   rain_2014,
   rain_2015,
   rain_2016,
   rain_2017,
   rain_2018,
   rain_2019,
   rain_2020,
   rain_2021)

str(climate_all)
## tidy up a bit...
climate_all <- climate_all %>% 
  dplyr::select(-x, -y)

### make dataset wider
climate_all_wider <- climate_all %>% 
  pivot_wider(
    names_from = year,
    values_from = rain)

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


#plots the results by year:
climate_all %>% 
  filter(merged_regions != "Not defined") %>% 
ggplot(aes(factor(year_as_double), rain, colour= merged_regions))+
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

climate_all %>% 
  filter(merged_regions != "Not defined") %>% 
  ggplot(aes(merged_regions, rain, colour= merged_regions))+
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


climate_all %>% 
  filter(merged_regions != "Not defined") %>% 
  ggplot(aes( rain))+
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

climate_all %>% 
  #filter(merged_regions != "Not defined") %>% 
  ggplot(aes( rain))+
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
str(climate_all_wider)
cluser_input <- climate_all_wider %>% 
  select(`2013`:`2021`  )
kmean_2 <- kmeans(cluser_input, centers = 2)

climate_all_wider <- climate_all_wider %>% 
  mutate(K_Cluster_2 = kmean_2$cluster)
names(climate_all_wider)

climate_all_wider %>% 
  ggplot(aes(x = X , y = Y, colour = factor(K_Cluster_2)))+
  geom_point()

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

###  silhouette width taking ages - it does!

# pam_K2 <- cluster::pam(cluser_input, k=3)
# pam_K2$silinfo$avg.width

sil_width <- map_dbl(2:5,function(k){
  model <- cluster::pam(x = cluser_input, k = k)  
  model$silinfo$avg.width})

sil_df <- data.frame(
  k =2:5,  
  sil_width = sil_width)

sil_df

# Plot the silhouette width Note higher sil width indicated better fit of model
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 1:5)

## This will tell me how may clusters are best 
## Then run this K.cluster and plot

## check how many clusters
kmean_2$cluster
# Cluster means
kmean_2$centers
# Cluster size
kmean_2$size
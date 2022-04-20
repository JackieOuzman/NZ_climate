

library(ggplot2)
library(dplyr)
library(tidyverse)

rain <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_climate_all_cluster_input.csv")
GST <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GST_climate_all_cluster_input.csv")
GDD <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GDD_climate_all_cluster_input.csv")
DOH <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOH_climate_all_cluster_input.csv")
DOF <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOF_climate_all_cluster_input.csv")
DOV <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOV_climate_all_cluster_input.csv")


names(rain)
rain <- rain %>% 
  dplyr::rename(
    "rain_2013"  = "X2013",      
    "rain_2014"  = "X2014",      
    "rain_2015"  ="X2015",      
    "rain_2016"  ="X2016" ,     
    "rain_2017"  ="X2017" ,    
    "rain_2018"  ="X2018" ,
    "rain_2019"  ="X2019" ,  
    "rain_2020"  ="X2020" , 
    "rain_2021"  ="X2021"
)


GST <- GST %>% 
  dplyr::rename(
    "GST_2013"  = "X2013",      
    "GST_2014"  = "X2014",      
    "GST_2015"  ="X2015",      
    "GST_2016"  ="X2016" ,     
    "GST_2017"  ="X2017" ,    
    "GST_2018"  ="X2018" ,
    "GST_2019"  ="X2019" ,  
    "GST_2020"  ="X2020" , 
    "GST_2021"  ="X2021"
  )

GDD <- GDD %>% 
  dplyr::rename(
    "GDD_2013"  = "X2013",      
    "GDD_2014"  = "X2014",      
    "GDD_2015"  ="X2015",      
    "GDD_2016"  ="X2016" ,     
    "GDD_2017"  ="X2017" ,    
    "GDD_2018"  ="X2018" ,
    "GDD_2019"  ="X2019" ,  
    "GDD_2020"  ="X2020" , 
    "GDD_2021"  ="X2021"
  )

DOH <- DOH %>% 
  dplyr::rename(
    "DOH_2013"  = "X2013",      
    "DOH_2014"  = "X2014",      
    "DOH_2015"  ="X2015",      
    "DOH_2016"  ="X2016" ,     
    "DOH_2017"  ="X2017" ,    
    "DOH_2018"  ="X2018" ,
    "DOH_2019"  ="X2019" ,  
    "DOH_2020"  ="X2020" , 
    "DOH_2021"  ="X2021"
  )

DOF <- DOF %>% 
  dplyr::rename(
    "DOF_2013"  = "X2013",      
    "DOF_2014"  = "X2014",      
    "DOF_2015"  ="X2015",      
    "DOF_2016"  ="X2016" ,     
    "DOF_2017"  ="X2017" ,    
    "DOF_2018"  ="X2018" ,
    "DOF_2019"  ="X2019" ,  
    "DOF_2020"  ="X2020" , 
    "DOF_2021"  ="X2021"
  )

DOV <- DOV %>% 
  dplyr::rename(
    "DOV_2013"  = "X2013",      
    "DOV_2014"  = "X2014",      
    "DOV_2015"  ="X2015",      
    "DOV_2016"  ="X2016" ,     
    "DOV_2017"  ="X2017" ,    
    "DOV_2018"  ="X2018" ,
    "DOV_2019"  ="X2019" ,  
    "DOV_2020"  ="X2020" , 
    "DOV_2021"  ="X2021"
  )


### Tidy up clms
names(DOV)
DOV <- DOV %>%  dplyr::select(-"ID",-"X" ,-"Y" ,-"OBJECTID",-"Region_Nam")
DOF <- DOF %>%  dplyr::select(-"ID",-"X" ,-"Y" ,-"OBJECTID",-"Region_Nam")
DOH <- DOH %>%  dplyr::select(-"ID",-"X" ,-"Y" ,-"OBJECTID",-"Region_Nam")
GDD <- GDD %>%  dplyr::select(-"ID",-"X" ,-"Y" ,-"OBJECTID",-"Region_Nam")
GST <- GST %>%  dplyr::select(-"ID",-"X" ,-"Y" ,-"OBJECTID",-"Region_Nam")


climate_all <- left_join(DOV, DOF)
climate_all <- left_join(climate_all, DOH)
climate_all <- left_join(climate_all, GDD)
climate_all <- left_join(climate_all, GST)
climate_all <- left_join(climate_all, rain)

names(climate_all)
climate_details <- climate_all %>% 
  dplyr::select("for_join", "ID":"Region_Nam")

climate_matrix <- climate_all %>% 
  dplyr::select("DOV_2013":"GST_2021", "rain_2013":"rain_2021")

climate_matrix_scale <- base::scale(climate_matrix)
#summary(climate_matrix_scale)

climate_matrix_scale_with_details <- cbind(climate_details, climate_matrix_scale)
names(climate_matrix_scale_with_details)

write.csv(
  climate_matrix_scale_with_details,
  paste0(
    "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
    "climate_all_year.csv"),
      row.names = FALSE
)


#############################################################################################################################################
#### Clusering using R data has no transformations - not sure if it needs to?
library(cluster)

####################################################################################
####    INPUT DATA    ##############################################################

##make a dataset we can work with this is scaled
str(climate_matrix)



####################################################################################
####    HOW MANY CLUSTERS    #######################################################

## elbow plot
library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = climate_matrix, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)



###  silhouette width taking ages - it does!


sil_width <- map_dbl(2:5,function(k){
  model <- cluster::pam(x = climate_matrix, k = k)  
  model$silinfo$avg.width})

sil_df <- data.frame(
  k =2:5,  
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
            "all_climate_how_many_clusters.csv"),
              row.names = FALSE) 


### Run solution from above analysis

## best number of clusters are:
k_mean_number <- 2


kmean_model <- kmeans(climate_matrix, centers = k_mean_number)

climate_all <- climate_all %>% 
  dplyr::mutate(k_cluster  = kmean_model$cluster)

names(climate_all)

### Plot and check to solution looks good



climate_all %>% 
  ggplot(aes(x = X , y = Y, colour = factor(k_cluster)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none"
        #legend.title = element_blank()
  )


write.csv(climate_all,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            "Climate_all_kmean.csv"),
          row.names = FALSE) 



## stats on the cluster solution



str(climate_all)

Kmean_2Clust_mean <- climate_all %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(DOV_2013:GST_2021, rain_2013:rain_2021),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )

Kmean_2Clust_SD<- climate_all %>% 
  group_by(k_cluster) %>% 
  summarise_at(vars(DOV_2013:GST_2021, rain_2013:rain_2021),sd, na.rm = TRUE)%>% 
  dplyr::mutate(stats = "SD" )

Kmean_2Clust_mean
Kmean_2Clust_SD
Kmean_2Clust_stats <- rbind(Kmean_2Clust_mean, Kmean_2Clust_SD)
Kmean_2Clust_stats

Kmean_2Clust_count<- climate_all %>% 
  group_by(k_cluster) %>% 
  summarise(count = n())

Kmean_2Clust_stats <- left_join(Kmean_2Clust_stats,Kmean_2Clust_count)
Kmean_2Clust_stats


write.csv(Kmean_2Clust_stats,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            "Climate_all_kmean_stats.csv"),
          row.names = FALSE) 


#################################################################################################################



number_clusters <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/all_climate_how_many_clusters.csv")

names(number_clusters)

number_clusters %>% 
  ggplot(aes(k, tot_withinss))+
  geom_line(size=3)+
  scale_x_continuous(breaks = 1:10)+
  theme_bw()+
  theme(plot.title = element_text(size=20,face="bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))+
  labs(x = "number of clusters",
       y = "sum of squared errors (SSE)",
       title = "Elbow Curve Method")

number_clusters %>% 
  filter(k < 6) %>% 
  filter(k > 1) %>% 
  ggplot(aes(k, sil_width))+
  geom_line(size=3)+
  scale_x_continuous(breaks = 2:5)+
  theme_bw()+
  theme(plot.title = element_text(size=20,face="bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))+
  labs(x = "number of clusters",
       y = "silhouette coefﬁcient",
       title = "Silhouette analysis")

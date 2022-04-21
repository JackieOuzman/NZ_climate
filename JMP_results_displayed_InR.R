## Plotting and understading the climate data from JMP


library(ggplot2)
library(dplyr)
library(tidyverse)



climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/JMP_climate_all_year_climate_all_year_with_cluster.csv")
names(climate_cluster)

cluster2 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster2)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster2
cluster3 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster3)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster3

cluster4 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster4)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster4
cluster5 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster5)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster5
cluster6 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster6)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster6
cluster7 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster7)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster7
cluster8 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster8)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster8
cluster9 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster9)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster9
cluster10 <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster10)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
cluster10

###### RAIN 7


Rain7cluster <- climate_cluster %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Rain7Cluster)))+
  geom_point()+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )
Rain7cluster



## stats on the cluster solution



str(climate_cluster)

Kmean_2Clust_mean <- climate_cluster %>% 
  group_by(Cluster2) %>% 
  summarise_at(vars(DOV_2013:rain_2021),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )

Kmean_2Clust_SD<- climate_cluster %>% 
  group_by(Cluster2) %>% 
  summarise_at(vars(DOV_2013:rain_2021),sd, na.rm = TRUE)%>% 
  dplyr::mutate(stats = "SD" )

Kmean_2Clust_mean
Kmean_2Clust_SD
Kmean_2Clust_stats <- rbind(Kmean_2Clust_mean, Kmean_2Clust_SD)
Kmean_2Clust_stats

Kmean_2Clust_count<- climate_cluster %>% 
  group_by(Cluster2) %>% 
  summarise(count = n())

Kmean_2Clust_stats <- left_join(Kmean_2Clust_stats,Kmean_2Clust_count)
Kmean_2Clust_stats


write.csv(Kmean_2Clust_stats,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            "All_climate",
            "_kmean_stats.csv"),
          row.names = FALSE) 
 


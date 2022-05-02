## Plotting and understading the climate data from JMP


library(ggplot2)
library(dplyr)
library(tidyverse)



climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/JMP_climate_all_year_climate_all_year_with_cluster.csv")
names(climate_cluster)
climate_cluster_rain_only <- climate_cluster %>% dplyr::select(for_join:Region_Nam, Rain7Cluster:Rain6Cluster)
climate_cluster_rain_only <- climate_cluster_rain_only %>% dplyr::select(-Rain7Distance,
                                                                         -Rain5Distance,
                                                                         -Rain4Distance,
                                                                         -Rain3Distance,
                                                                         -Rain2Distance
                                                                         )



names(climate_cluster_rain_only)

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


#############################################################################################################
#Just the rainfall cluster solutions



names(climate_cluster_rain_only)


climate_cluster_rain_only_narrow <-climate_cluster_rain_only %>% 
    pivot_longer(
    cols= c(Rain7Cluster:Rain6Cluster),
    names_to = "cluster_solution",
    values_to = "cluster"
  )
head(climate_cluster_rain_only_narrow)


climate_cluster_rain_only_narrow %>% 
  filter(cluster_solution == "Rain7Cluster") %>% 
  ggplot(aes(x = X , y = Y, colour = factor(cluster)))+
  geom_point()+
  #facet_wrap(.~ cluster_solution)+
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

## stats on the rain cluster solution 7

climate_cluster_rain_only_for_stats <- climate_cluster_rain_only

str(climate_cluster_rain_only_for_stats)
rain_all_year <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_kmean_2.csv")
rain_all_year <- rain_all_year %>%  dplyr::select(for_join, X2013:X2021)
str(rain_all_year)

str(rain_all_year)
str(climate_cluster_rain_only_for_stats)
climate_cluster_rain_only_for_stats <- left_join(rain_all_year, climate_cluster_rain_only_for_stats)

names(climate_cluster_rain_only_for_stats)

Kmean_7Clust_mean <- climate_cluster_rain_only_for_stats %>% 
  group_by(Rain7Cluster) %>% 
  summarise_at(vars(X2013:X2021),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )

Kmean_7Clust_SD<- climate_cluster_rain_only_for_stats %>% 
  group_by(Rain7Cluster) %>% 
  summarise_at(vars(X2013:X2021),sd, na.rm = TRUE)%>% 
  dplyr::mutate(stats = "SD" )

Kmean_7Clust_mean
Kmean_7Clust_SD
Kmean_7Clust_stats <- rbind(Kmean_7Clust_mean, Kmean_7Clust_SD)
Kmean_7Clust_stats

Kmean_7Clust_count<- climate_cluster_rain_only_for_stats %>% 
  group_by(Rain7Cluster) %>% 
  summarise(count = n())

Kmean_7Clust_stats <- left_join(Kmean_7Clust_stats,Kmean_7Clust_count)
Kmean_7Clust_stats


write.csv(Kmean_7Clust_stats,
          paste0(
            "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            "Rain7Cluster",
            "_kmean_stats.csv"),
          row.names = FALSE) 



#############################################################################################################
#Just the GDD cluster solutions

## how many clusters?

CCC_GDD_df <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_cluster_JMP_report_GDD_only.csv")
str(CCC_GDD_df)                  

CCC_GDD_df %>% 
  ggplot(aes(NCluster, CCC))+
  geom_line(size=3)+
  scale_x_continuous(breaks = 1:10)+
  theme_bw()+
  theme(plot.title = element_text(size=20,face="bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))+
  labs(x = "number of clusters",
       y = "Cubic Cluster Criterion",
       title = "CCC method")


names(climate_cluster_rain_only)


climate_cluster_GDD_only_narrow <-climate_cluster_GDD_only %>% 
  pivot_longer(
    cols= c(Rain7Cluster:Rain6Cluster),
    names_to = "cluster_solution",
    values_to = "cluster"
  )
head(climate_cluster_GDD_only_narrow)


climate_cluster_GDD_only_narrow %>% 
  filter(cluster_solution == "GDD7Cluster") %>% 
  ggplot(aes(x = X , y = Y, colour = factor(cluster)))+
  geom_point()+
  #facet_wrap(.~ cluster_solution)+
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

## stats on the rain cluster solution 7


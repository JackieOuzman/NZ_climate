## Plotting and understading the climate data this is the results of clustering done in JMP


library(ggplot2)
library(dplyr)
library(tidyverse)










number_clusters_Jmp <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_cluster_JMP_report_GST_only.csv")

names(number_clusters_Jmp)



number_clusters_Jmp %>% 
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





climate_cluster <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_all_year_climate_all_year_with_cluster.csv")
str(climate_cluster$GST)
names(climate_cluster)

GST_climate_cluster <- climate_cluster %>% 
  dplyr::select(for_join:Region_Nam,
                GST_2Cluster,
                GST_3Cluster,
                GST_4Cluster,
                GST_5Cluster,
                GST_6Cluster,
                GST_7Cluster,
                GST_8Cluster,
                GST_9Cluster,
                GST_10Cluster)
str(GST_climate_cluster)

## make df into long format
GST_climate_cluster_narrow <- GST_climate_cluster %>% 
pivot_longer(cols= GST_2Cluster:GST_10Cluster,
             names_to = "Cluster",
             values_to = "Cluster_solution")

### change the order of the cluster in the df
unique(GST_climate_cluster_narrow$Cluster_solution)

GST_climate_cluster_narrow$Cluster <- factor(GST_climate_cluster_narrow$Cluster, 
                                                      levels=c("GST_2Cluster",
                                                               "GST_3Cluster",
                                                               "GST_4Cluster",
                                                               "GST_5Cluster",
                                                               "GST_6Cluster",
                                                               "GST_7Cluster",
                                                               "GST_8Cluster",
                                                               "GST_9Cluster",
                                                               "GST_10Cluster"
                                                               ))


### Plot all the cluster solutions

GST_climate_cluster_narrow %>% 
  ggplot(aes(x = X , y = Y, colour = factor(Cluster_solution)))+
  geom_point()+
  theme_bw()+
  facet_wrap(.~Cluster)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
        )


#### summary stats on GST cluster solutions

climate_grid_values <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/Climate_all_JMP_IN.csv")

# summary(climate_grid_values)
# names(climate_grid_values)


climate_grid_values <- climate_grid_values %>%
  dplyr::select(for_join,
                GST_2013:GST_2021)

### join it to the wide data set
names(GST_climate_cluster)
names(climate_grid_values)

climate_grid_for_stats <- left_join(climate_grid_values, GST_climate_cluster)

names(climate_grid_for_stats)

Kmean_Clust_mean <- climate_grid_for_stats %>% 
  group_by(GST_8Cluster) %>% 
  summarise_at(vars(GST_2013:GST_2021),mean, na.rm = TRUE) %>% 
  dplyr::mutate(stats = "mean" )


Kmean_Clust_count<- climate_grid_for_stats %>% 
  group_by(GST_8Cluster) %>% 
  summarise(count = n())

Kmean_Clust_stats <- left_join(Kmean_Clust_mean,Kmean_Clust_count)
Kmean_Clust_stats


write.csv(Kmean_Clust_stats,
          paste0(
            "V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
            "GST",
            "_8kmean_stats.csv"),
          row.names = FALSE) 

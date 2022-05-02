## Plotting and understading the climate data


library(ggplot2)
library(dplyr)
library(tidyverse)






m


#number_clusters <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_how_many_k.csv")
#number_clusters <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/rainfall_scaled_how_many_k.csv")
number_clusters_Jmp <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_cluster_JMP_report_GDD_only.csv")

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

str(climate)

#### UP TO HERE#####

### how do I get this value

#climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_kmean_2.csv")
#climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GST_kmean.csv")
climate_cluster <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GDD_kmean.csv")
#climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOH_kmean.csv")
#climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOF_kmean.csv")
#climate_cluster <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/DOV_kmean.csv")
names(climate_cluster)

climate_cluster %>% 
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



## Plotting and understading the climate data


library(ggplot2)
library(dplyr)
library(tidyverse)

climate <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_climate_all.csv")

# what is the avearge for the whole region
summary(climate$rain)

# what is the avearge for the whole region by year
climate %>% group_by(year) %>%
  summarise(mean = mean(rain, na.rm = FALSE),
            Median = median(rain, na.rm = FALSE))
names(climate)
climate %>% 
  ggplot(aes( rain))+
  geom_histogram(alpha = .5)+
  facet_wrap(.~ factor(year_as_double))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  geom_vline(xintercept = 508, linetype="dashed", 
             color = "blue", size=0.8)

# what is the avearge for the sub region
mean_values_sub_region_by_yr <- climate %>% group_by(merged_regions, year) %>%
  summarise(mean = mean(rain, na.rm = FALSE)) %>% 
  filter(merged_regions!= "Not defined")
mean_values_sub_region_by_yr$year <- as.factor(mean_values_sub_region_by_yr$year)

mean_values_detailed_sub_region_by_yr <- climate %>% group_by(Region_Nam, year) %>%
  summarise(mean = mean(rain, na.rm = FALSE)) %>% 
  filter(Region_Nam!= "") %>% 
  
mean_values_detailed_sub_region_by_yr
mean_values_detailed_sub_region_by_yr$year <- as.factor(mean_values_detailed_sub_region_by_yr$year)

unique(mean_values_detailed_sub_region_by_yr$Region_Nam)

mean_values_sub_region_by_yr %>% 
  ggplot(aes(year, mean, fill= merged_regions))+
  geom_col(position="dodge")


## change the order of the regions

mean_values_detailed_sub_region_by_yr$Region_Nam <-
  factor(
    mean_values_detailed_sub_region_by_yr$Region_Nam,
    levels = c(
      "Wairau Valley (West of Narrows)",
      "Wairau Plain (East of Narrows)",
      "Awatere",
      "Grassmere, Flaxbourne, Kekerengu"
    )
  )




mean_values_detailed_sub_region_by_yr %>% 
  ggplot(aes(year, mean, fill= Region_Nam))+
  geom_col(position="dodge")+
  theme_bw()+
  theme(legend.title = element_blank())+
  geom_hline(yintercept = 508, linetype="dashed", 
                       color = "blue", size=0.8)



climate %>% 
  filter(merged_regions!= "Not defined") %>% 
  ggplot(aes( rain, fill = merged_regions))+
  geom_histogram(alpha = .5)+
  facet_wrap(.~ factor(year_as_double))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  geom_vline(xintercept = 532, linetype="dashed", #Awatere and South 
             color = "#F8766D", size=0.8)+
  geom_vline(xintercept = 498, linetype="dashed", 
             color = "sky blue", size=0.8)


number_clusters <- read.csv("V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/GS_rainfall_how_many_k.csv")

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
  
str(climate)

climate %>% 
  ggplot(aes(x = X , y = Y, colour = factor(K_Cluster_2)))+
  geom_point()











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
climate_matrix <- climate_all %>% 
  dplyr::select("DOV_2013":"GST_2021", "rain_2013":"rain_2021")

base::scale(climate_all)

write.csv(
  climate_all,
  paste0(
    "V:/Viticulture/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/",
    "climate_all_year.csv"),
      row.names = FALSE
)
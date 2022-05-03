## Plotting and understading the climate data this is the results of clustering done in JMP


library(ggplot2)
library(dplyr)
library(tidyverse)


#### modelled data

climate_grid_values <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/Climate_all_JMP_IN.csv")

# summary(climate_grid_values)
names(climate_grid_values)


climate_grid_values <- climate_grid_values %>%
  dplyr::select(for_join:Region_Nam,
                DOV_2013:DOH_2021)


names(climate_grid_values)

### needs to be long format
climate_grid_values_narrow <- climate_grid_values
pivot_longer()
